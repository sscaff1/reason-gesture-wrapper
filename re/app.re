open BsReactNative;

/*  childSize={width: 300., height: 300.} */
let default = () =>
  <GestureHandler childSize={width: 200., height: 200.}>
    <Image
      style=Style.(style([height(Pt(200.)), width(Pt(200.))]))
      source=(
        URI(Image.imageURISource(~uri="https://picsum.photos/200", ()))
      )
    />
  </GestureHandler>;