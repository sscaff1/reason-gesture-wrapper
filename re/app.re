open BsReactNative;

/*  childSize={width: 300., height: 300.} */
let default = () =>
  <View>
    <GestureHandlerWithState childSize={width: 300., height: 300.}>
      <Image
        style=Style.(style([height(Pt(300.)), width(Pt(300.))]))
        source=(
          URI(Image.imageURISource(~uri="https://picsum.photos/300", ()))
        )
      />
    </GestureHandlerWithState>
    <GestureHandlerWithState childSize={width: 200., height: 200.}>
      <Image
        style=Style.(style([height(Pt(200.)), width(Pt(200.))]))
        source=(
          URI(Image.imageURISource(~uri="https://picsum.photos/200", ()))
        )
      />
    </GestureHandlerWithState>
  </View>;