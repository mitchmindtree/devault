use devault::Devault;

#[test]
fn test_unit() {
    #[derive(Devault)]
    struct Foo;
    let _: Foo = Foo::default();
}

#[test]
fn test_no_attr() {
    #[derive(Debug, Devault)]
    struct Foo(u8);
    #[derive(Debug, Default)]
    struct Bar(u8, f32, i64);

    let _ = Foo::default();
    let _ = Bar::default();

    #[derive(Debug, Devault)]
    struct Baz {
        a: u8,
    }
    #[derive(Debug, Default)]
    struct Qux {
        a: u8,
        b: i64,
    }

    let baz = Baz::default();
    let qux = Qux::default();

    assert_eq!(baz.a, qux.a);
}

#[test]
fn test_attr() {
    const B: u32 = 10;

    #[derive(Debug, Devault, PartialEq)]
    struct Foo {
        #[devault("1.0")]
        a: f32,
        #[devault("B")]
        b: u32,
        #[devault("0.5")]
        c: f32,
    }

    let expected = Foo {
        a: 1.0,
        b: B,
        c: 0.5,
    };
    let foo = Foo::default();
    assert_eq!(foo, expected);

    fn foo_zero() -> Foo {
        Foo {
            a: 0.0,
            b: 0,
            c: 0.0,
        }
    }

    #[derive(Debug, Devault, PartialEq)]
    struct Bar {
        f: Foo,
        #[devault("foo_zero()")]
        g: Foo,
    }

    let bar = Bar::default();
    assert_eq!(bar.f, foo);
    assert!(bar.g != foo);
}

#[test]
fn test_nested_attrs() {
    #[derive(Debug, Devault, PartialEq)]
    struct Foo {
        #[devault("1.0", constant)]
        a: f32,
        #[devault("10", function)]
        b: u32,
        #[devault("0.5", constant = "INIT_C", function = "start_c")]
        c: f32,
    }
    assert_eq!(Foo::DEFAULT_A, 1.0);
    assert_eq!(Foo::default_b(), 10);
    assert_eq!(Foo::INIT_C, 0.5);
    assert_eq!(Foo::start_c(), 0.5);
}
