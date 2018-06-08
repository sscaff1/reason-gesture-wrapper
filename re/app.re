open BsReactNative;

/*  childSize={width: 300., height: 300.} */
let default = () =>
  <GestureHandlerWithState childSize={width: 300., height: 300.}>
    <Image
      style=Style.(style([height(Pt(300.)), width(Pt(300.))]))
      source=(
        URI(Image.imageURISource(~uri="https://picsum.photos/300", ()))
      )
    />
  </GestureHandlerWithState>;