open EndianString

module BE = BigEndian
module LE = LittleEndian

let s = String.make 10 '\x00'

let assert_bound_check2 f v1 v2 =
  try
    ignore(f v1 v2);
    assert false
  with
     | Invalid_argument("index out of bounds") -> ()

let assert_bound_check3 f v1 v2 v3 =
  try
    ignore(f v1 v2 v3);
    assert false
  with
     | Invalid_argument("index out of bounds") -> ()

let test () =
  assert_bound_check2 BE.get_uint16 s (-1);
  assert_bound_check2 BE.get_uint16 s 9;
  assert_bound_check2 BE.get_int32 s (-1);
  assert_bound_check2 BE.get_int32 s 7;
  assert_bound_check2 BE.get_int64 s (-1);
  assert_bound_check2 BE.get_int64 s 3;

  assert_bound_check3 BE.set_int16 s (-1) 0;
  assert_bound_check3 BE.set_int16 s 9 0;
  assert_bound_check3 BE.set_int32 s (-1) 0l;
  assert_bound_check3 BE.set_int32 s 7 0l;
  assert_bound_check3 BE.set_int64 s (-1) 0L;
  assert_bound_check3 BE.set_int64 s 3 0L;

  BE.set_int16 s 0 0x1234;
  assert( BE.get_uint16 s 0 = 0x1234 );
  assert( BE.get_uint16 s 1 = 0x3400 );
  assert( BE.get_uint16 s 2 = 0 );

  assert( LE.get_uint16 s 0 = 0x3412 );
  assert( LE.get_uint16 s 1 = 0x0034 );
  assert( LE.get_uint16 s 2 = 0 );

  assert( BE.get_int16 s 0 = 0x1234 );
  assert( BE.get_int16 s 1 = 0x3400 );
  assert( BE.get_int16 s 2 = 0 );

  BE.set_int16 s 0 0xFEDC;
  assert( BE.get_uint16 s 0 = 0xFEDC );
  assert( BE.get_uint16 s 1 = 0xDC00 );
  assert( BE.get_uint16 s 2 = 0 );

  assert( LE.get_uint16 s 0 = 0xDCFE );
  assert( LE.get_uint16 s 1 = 0x00DC );
  assert( LE.get_uint16 s 2 = 0 );

  assert( BE.get_int16 s 0 = -292 );
  assert( BE.get_int16 s 1 = -9216 );
  assert( BE.get_int16 s 2 = 0 );

  LE.set_int16 s 0 0x1234;
  assert( BE.get_uint16 s 0 = 0x3412 );
  assert( BE.get_uint16 s 1 = 0x1200 );
  assert( BE.get_uint16 s 2 = 0 );

  LE.set_int16 s 0 0xFEDC;
  assert( LE.get_uint16 s 0 = 0xFEDC );
  assert( LE.get_uint16 s 1 = 0x00FE );
  assert( LE.get_uint16 s 2 = 0 );

  BE.set_int32 s 0 0x12345678l;
  assert( BE.get_int32 s 0 = 0x12345678l );
  assert( LE.get_int32 s 0 = 0x78563412l );

  LE.set_int32 s 0 0x12345678l;
  assert( LE.get_int32 s 0 = 0x12345678l );
  assert( BE.get_int32 s 0 = 0x78563412l );

  BE.set_int64 s 0 0x1234567890ABCDEFL;
  assert( BE.get_int64 s 0 = 0x1234567890ABCDEFL );
  assert( LE.get_int64 s 0 = 0xEFCDAB9078563412L );

  LE.set_int64 s 0 0x1234567890ABCDEFL;
  assert( LE.get_int64 s 0 = 0x1234567890ABCDEFL );
  assert( BE.get_int64 s 0 = 0xEFCDAB9078563412L )

