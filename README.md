# Binmarshal
[![crates.io](https://img.shields.io/crates/v/binmarshal.svg)](https://crates.io/crates/binmarshal)
[![Documentation](https://docs.rs/binmarshal/badge.svg)](https://docs.rs/binmarshal)

Pack and unpack structs and enums into and out of binary data streams.

## Add to your project
```
cargo add binmarshal
```

## Packing Structs and Enums
```rust
#[derive(Debug, Clone, BinMarshal)]
#[marshal(tag_type = u8)]
enum MyEnum {
  #[marshal(tag = "0")]
  Variant1,
  #[marshal(tag = "1")]
  Variant2 {
    #[marshal(bits = 4)]
    a: u8,
    #[marshal(align = 1)]
    b: u16,
    c: LengthTaggedVec<u8, i32>
  }
}

fn main() {
  let v = MyEnum::Variant2 {
    a: 13,
    b: 5192,
    c: LengthTaggedVec::new(vec![ -12, -242, 12034 ])
  }

  // Packing
  let mut bytes = [0u8; 256];
  let mut writer = BufferBitWriter::new(&mut bytes);
  v.write(&mut writer, ());
  let slice = writer.slice();   // Gives you a &[u8] reference of the appropriate length, as a subset of "bytes"

  // Unpacking
  let v = MyEnum::read(&mut BitView::new(slice), ()).unwrap();
}
```

## Using Context
Context allows you to pass variables between structs and enums, primarily for tagging enums. If you are using tagged enums with context, you must call `.update()` prior to writing.

```rust
#[derive(Context)]
struct MyContext {
  tag: u8
}

#[derive(Debug, Clone, BinMarshal)]
#[marshal(ctx = MyContext, tag_type = u8, tag = "ctx.tag")]
enum MyEnum {
  #[marshal(tag = "1")]
  Variant1,
  #[marshal(tag = "2")]
  Variant2,
}

#[derive(Debug, Clone, BinMarshal)]
struct MyStruct {
  variant: u8,
  #[marshal(ctx = "{ tag: variant }")]
  inner: MyEnum
}
```

## Examples
More complex examples are provided under [examples](https://github.com/GrappleRobotics/binmarshal/tree/master/examples)