#include <memory.h>
#include <stdbool.h>
#include <stdio.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/intext.h>
#include <caml/memory.h>

/* [Mem.t] and [Ptr.t] support marshalling with sharing.  Each call to [Marshal.to_*] is
   assigned a new [serialize_count].  Serializing [Mem.t] and [Ptr.t] stores the
   underlying array of structs only if it wasn't already serialized for the current
   [serialize_count].  Otherwise, it stores the reference to the one already serialized.

   Deserialization [Mem.t] and [Ptr.t] has to deserialize the underlying array of structs
   or use one already deserialized. */

void hdf5_mem_finalize(value);
void hdf5_mem_serialize(value, uintnat*, uintnat*);
uintnat hdf5_mem_deserialize(void*);

static struct custom_operations hdf5_mem_ops = {
  "hdf5.struct.mem",
  hdf5_mem_finalize,
  custom_compare_default,
  custom_hash_default,
  hdf5_mem_serialize,
  hdf5_mem_deserialize,
  custom_compare_ext_default,
#ifdef custom_fixed_length_default
  custom_fixed_length_default,
#endif
};

struct hdf5_caml_mem {
  size_t refcount;
  void *data;
  /* The following fields are stored as value since they're mostly used from the OCaml
     code.  They are always integers so it's not a problem that they're not tracked by the
     GC. */
  /* The number of elements that [data] is allocated for. */
  value capacity;
  /* The number of elements allowed to be accessed in [data].  It is less or equal than
     [capacity]. */
  value nmemb;
  /* The size of a single element stored in [data]. */
  value size;
  /* Contains the last [serialize_count] for which this struct was serialized. */
  unsigned long serialize_count;
  /* The ID of this struct in the latest serialization. */
  unsigned long serialize_id;
};

#define Mem_val(v) *((struct hdf5_caml_mem**) Data_custom_val(v))

#define CAML_HDF5_MAX_MEMORY (1024 * 1024 * 1024)

/* The current amount of memory allocated. */
static size_t memory_allocated = 0;

void caml_hdf5_raise_out_of_memory()
{
  char msg[256];
  snprintf(msg, sizeof(msg), "Out of memory: %ld", memory_allocated);
  caml_failwith(msg);
}

value hdf5_caml_struct_mem_create(value capacity_v, value size_v)
{
  CAMLparam2(capacity_v, size_v);
  CAMLlocal1(v);
  void *data;
  size_t capacity, size;
  struct hdf5_caml_mem *mem;

  capacity = Long_val(capacity_v);
  size = Long_val(size_v);

  data = calloc(capacity, size);
  if (data == NULL)
    caml_hdf5_raise_out_of_memory();

  mem = malloc(sizeof(struct hdf5_caml_mem));
  if (mem == NULL)
  {
    free(data);
    caml_hdf5_raise_out_of_memory();
  }
  memory_allocated += capacity * size;

  mem->data = data;
  mem->refcount = 1;
  mem->capacity = capacity_v;
  mem->nmemb = capacity_v;
  mem->size = size_v;
  mem->serialize_count = 0;
  mem->serialize_id = 0;

  v =
    caml_alloc_custom(
      &hdf5_mem_ops, sizeof(struct hdf5_caml_mem*), capacity * size,
      CAML_HDF5_MAX_MEMORY);
  Mem_val(v) = mem;

  CAMLreturn(v);
}

value hdf5_caml_struct_mem_of_mem(struct hdf5_caml_mem *mem)
{
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_custom(&hdf5_mem_ops, sizeof(struct hdf5_caml_mem*), 0, 1);
  Mem_val(v) = mem;
  mem->refcount++;
  CAMLreturn(v);
}

void hdf5_mem_finalize(value v)
{
  struct hdf5_caml_mem *mem;

  mem = Mem_val(v);
  if (--mem->refcount == 0)
  {
    memory_allocated -= Long_val(mem->capacity) * Long_val(mem->size);
    free(mem->data);
    free(mem);
  }
}

static unsigned long serialize_count = 0;
static unsigned long serialize_id = 0;

void serialize_mem(struct hdf5_caml_mem *mem)
{
  size_t nmemb, size;

  /* Check whether this [hdf5_caml_mem] was already serialized in this serialization
     session.  If so, only store its ID.  Otherwise assign it a new ID and store the whole
     structure. */
  if (mem->serialize_count == serialize_count)
  {
    caml_serialize_int_8(mem->serialize_id);
  }
  else
  {
    mem->serialize_count = serialize_count;
    mem->serialize_id = serialize_id++;
    nmemb = Long_val(mem->nmemb);
    size  = Long_val(mem->size);
    caml_serialize_int_8(mem->serialize_id);
    caml_serialize_int_8(Long_val(mem->capacity));
    caml_serialize_int_8(nmemb);
    caml_serialize_int_8(size);
    caml_serialize_block_1(mem->data, nmemb * size);
  }
}

void hdf5_mem_serialize(value v, uintnat *wsize_32, uintnat *wsize_64)
{
  serialize_mem(Mem_val(v));
  *wsize_32 = sizeof(struct hdf5_caml_mem*);
  *wsize_64 = sizeof(struct hdf5_caml_mem*);
}

static struct hdf5_caml_mem **mems = NULL;
static size_t mems_capacity = 0;
static unsigned long deserialize_id = 0;

struct hdf5_caml_mem* deserialize_mem()
{
  struct hdf5_caml_mem *mem;
  unsigned long id;
  size_t capacity, nmemb, size;
  void *data;

  id = caml_deserialize_uint_8();
  /* If we have already deserialized this [hdf5_caml_mem] just fetch it.  IDs come in
     sequence, so comparing it to the largest deserialized ID checks whether it was
     already deserialized. */
  if (id < deserialize_id)
    mem = mems[id];
  else
  {
    deserialize_id = id + 1;
    if (id >= mems_capacity)
    {
      mems_capacity = id * 2;
      mems = realloc(mems, mems_capacity * sizeof(struct hdf5_caml_mem*));
      if (mems == NULL)
        caml_hdf5_raise_out_of_memory();
    }
    capacity = caml_deserialize_uint_8();
    nmemb    = caml_deserialize_uint_8();
    size     = caml_deserialize_uint_8();
    mem = malloc(sizeof(struct hdf5_caml_mem));
    if (mem == NULL)
      caml_hdf5_raise_out_of_memory();
    data = calloc(capacity, size);
    if (data == NULL)
    {
      free(mem);
      caml_hdf5_raise_out_of_memory();
    }
    memory_allocated += capacity * size;
    caml_deserialize_block_1(data, nmemb * size);
    mem->refcount = 0;
    mem->data = data;
    mem->capacity = Val_long(capacity);
    mem->nmemb = Val_long(nmemb);
    mem->size = Val_long(size);
    mem->serialize_count = 0;
    mem->serialize_id = 0;
    mems[id] = mem;
  }

  return mem;
}

uintnat hdf5_mem_deserialize(void *dst)
{
  struct hdf5_caml_mem *mem;

  mem = deserialize_mem();
  mem->refcount++;
  *((struct hdf5_caml_mem**) dst) = mem;
  return sizeof(struct hdf5_caml_mem*);
}

void hdf5_caml_struct_mem_write_buf(
  struct hdf5_caml_mem *mem, value buf_v, value dst_pos_v)
{
  memcpy(
    (char*) Caml_ba_data_val(buf_v) + Long_val(dst_pos_v),
    mem->data,
    Long_val(mem->nmemb) * Long_val(mem->size));
}

void hdf5_caml_struct_mem_read_buf(
  struct hdf5_caml_mem *mem, value buf_v, value dst_pos_v)
{
  memcpy(
    mem->data,
    (char*) Caml_ba_data_val(buf_v) + Long_val(dst_pos_v),
    Long_val(mem->nmemb) * Long_val(mem->size));
}

void hdf5_caml_struct_mem_realloc(value t_v, value capacity_v)
{
  CAMLparam2(t_v, capacity_v);
  struct hdf5_caml_mem *mem;
  void *data;

  mem = Mem_val(t_v);
  data = realloc(mem->data, Long_val(capacity_v) * Long_val(mem->size));
  if (data == NULL)
    caml_hdf5_raise_out_of_memory();
  mem->data = data;
  mem->capacity = capacity_v;
  CAMLreturn0;
}

void hdf5_caml_struct_mem_blit(
  value src_v, value src_pos_v, value dst_v, value dst_pos_v, value len_v)
{
  CAMLparam3(src_v, dst_v, len_v);
  struct hdf5_caml_mem *src, *dst;
  size_t size;

  src = Mem_val(src_v);
  dst = Mem_val(dst_v);
  size = Long_val(src->size);
  memcpy(
    (char*) dst->data + Long_val(src_pos_v) * size,
    (char*) src->data + Long_val(dst_pos_v) * size,
    Long_val(len_v) * size);
  CAMLreturn0;
}

void hdf5_ptr_finalize(value v);
void hdf5_ptr_serialize(value, uintnat*, uintnat*);
uintnat hdf5_ptr_deserialize(void*);

static struct custom_operations hdf5_ptr_ops = {
  "hdf5.struct.ptr",
  hdf5_ptr_finalize,
  custom_compare_default,
  custom_hash_default,
  hdf5_ptr_serialize,
  hdf5_ptr_deserialize,
  custom_compare_ext_default,
#ifdef custom_fixed_length_default
  custom_fixed_length_default,
#endif
};

struct hdf5_caml_ptr {
  void *ptr;
  struct hdf5_caml_mem *mem;
  value pos;
};

#define Ptr_val(v) ((struct hdf5_caml_ptr*) Data_custom_val(v))

value hdf5_caml_struct_ptr_create(value mem_v, value pos_v)
{
  CAMLparam2(mem_v, pos_v);
  CAMLlocal1(v);
  struct hdf5_caml_mem *mem;
  struct hdf5_caml_ptr *ptr;

  mem = Mem_val(mem_v);
  v = caml_alloc_custom(&hdf5_ptr_ops, sizeof(struct hdf5_caml_ptr), 0, 1);
  ptr = Ptr_val(v);
  ptr->ptr = (char*) mem->data + Long_val(pos_v) * Long_val(mem->size);
  ptr->mem = mem;
  ptr->pos = pos_v;
  mem->refcount++;

  CAMLreturn(v);
}

void hdf5_ptr_finalize(value v)
{
  struct hdf5_caml_mem *mem;

  mem = Ptr_val(v)->mem;
  if (--mem->refcount == 0)
  {
    free(mem->data);
    free(mem);
  }
}

void hdf5_ptr_serialize(value v, uintnat *wsize_32, uintnat *wsize_64)
{
  struct hdf5_caml_ptr *ptr;

  ptr = Ptr_val(v);
  caml_serialize_int_8(Long_val(ptr->pos));
  serialize_mem(ptr->mem);
  *wsize_32 = sizeof(struct hdf5_caml_ptr);
  *wsize_64 = sizeof(struct hdf5_caml_ptr);
}

uintnat hdf5_ptr_deserialize(void *dst)
{
  struct hdf5_caml_mem *mem;
  struct hdf5_caml_ptr *ptr;
  unsigned long pos;

  ptr = dst;
  pos = caml_deserialize_sint_8();
  mem = deserialize_mem();
  mem->refcount++;
  ptr->ptr = (char*) mem->data + pos * Long_val(mem->size);
  ptr->mem = mem;
  ptr->pos = Val_long(pos);

  return sizeof(struct hdf5_caml_ptr);
}

static bool initialized = false;

void hdf5_initialize()
{
  initialized = true;
  caml_register_custom_operations(&hdf5_mem_ops);
  caml_register_custom_operations(&hdf5_ptr_ops);
}

void hdf5_caml_struct_reset_serialize()
{
  if (!initialized)
    hdf5_initialize();
  serialize_count++;
  serialize_id = 0;
}

void hdf5_caml_struct_reset_deserialize()
{
  CAMLparam0();
  if (!initialized)
    hdf5_initialize();
  if (mems_capacity == 0)
  {
    mems_capacity = 256;
    mems = calloc(mems_capacity, sizeof(struct hdf5_caml_mem*));
    if (mems == NULL)
      caml_hdf5_raise_out_of_memory();
  }
  deserialize_id = 0;
  CAMLreturn0;
}
