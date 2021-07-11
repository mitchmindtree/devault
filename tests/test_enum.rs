use devault::Devault;

#[test]
fn test_attr() {
    #[derive(Debug, Devault, PartialEq)]
    #[devault("Foo::B(128)")]
    #[allow(dead_code)]
    enum Foo {
        A,
        B(u8),
    }
    assert_eq!(Foo::default(), Foo::B(128));
}

#[test]
fn test_nested_attrs() {
    #[derive(Debug, Devault, PartialEq)]
    #[devault("Foo::C(1.0)", constant)]
    #[allow(dead_code)]
    enum Foo {
        A,
        B,
        C(f32),
    }
    assert_eq!(Foo::default(), Foo::C(1.0));
    assert_eq!(Foo::DEFAULT, Foo::C(1.0));

    #[derive(Debug, Devault, PartialEq)]
    #[devault("Bar::B", constant = "INIT")]
    #[allow(dead_code)]
    enum Bar {
        A,
        B,
    }
    assert_eq!(Bar::default(), Bar::B);
    assert_eq!(Bar::INIT, Bar::B);
}
