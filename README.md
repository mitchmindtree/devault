# devault [![Actions Status](https://github.com/mitchmindtree/devault/workflows/devault/badge.svg)](https://github.com/mitchmindtree/devault/actions) [![Crates.io](https://img.shields.io/crates/v/devault.svg)](https://crates.io/crates/devault) [![Crates.io](https://img.shields.io/crates/l/devault.svg)](https://github.com/mitchmindtree/devault/blob/master/LICENSE-MIT) [![docs.rs](https://docs.rs/devault/badge.svg)](https://docs.rs/devault/)

A more flexible alternative to deriving `Default`.

Deriving `Devault` behaves the same as deriving `Default`, but includes some added benefits.

# Added Benefits

**devault** allows for specifying specific default values for fields, even if `Default` is not
implemented for their respective type.

```rust
use devault::Devault;

const C: u32 = 10;

#[derive(Debug, Devault, PartialEq)]
struct Foo {
    a: u8,
    #[devault("1.0")]
    b: f32,
    #[devault("C")]
    c: u32,
    #[devault("Bar(0.5)")]
    d: Bar,
}

#[derive(Debug, PartialEq)]
struct Bar(f32);

fn main() {
    let foo = Foo::default();
    assert_eq!(foo.a, 0);
    assert_eq!(foo.b, 1.0);
    assert_eq!(foo.c, C);
    assert_eq!(foo.d, Bar(0.5));
}
```

It can be derived for enums too, with the requirement that a default value is provided.

```rust
use devault::Devault;

#[derive(Debug, Devault, PartialEq)]
#[devault("Foo::B(128)")]
enum Foo {
    A,
    B(u8),
}

fn main() {
    assert_eq!(Foo::default(), Foo::B(128));
}
```

**devault** can generate associated constants and/or functions for constructing a field's
default value outside of the `Default` implementation.

```rust
use devault::Devault;

#[derive(Debug, Devault, PartialEq)]
struct Foo {
    #[devault("1.0", constant)]
    a: f32,
    #[devault("10", function)]
    b: u32,
    #[devault("0.5", constant = "INIT_C", function = "start_c")]
    c: f32,
}

#[derive(Debug, Devault, PartialEq)]
#[devault("Bar::B(42)", constant)]
enum Bar {
    A,
    B(u8),
}

fn main() {
    assert_eq!(Foo::DEFAULT_A, 1.0);
    assert_eq!(Foo::default_b(), 10);
    assert_eq!(Foo::INIT_C, 0.5);
    assert_eq!(Foo::start_c(), 0.5);
    assert_eq!(Bar::DEFAULT, Bar::B(42));
}
```

# TODO

- Support generic types.
