open BsReactNative;

type coordinates = {
  x: float,
  y: float,
};

type size = {
  width: float,
  height: float,
};

type gestureTypes =
  | Corner(coordinates)
  | TopBottom(float)
  | LeftRight(float)
  | TouchMovement;

type state = {
  pan: Animated.ValueXY.t,
  panResponder: PanResponder.t,
  childCoordinates: ref(Animated.ValueXY.jsValue),
  panListener: string,
};

let resetPan = pan => Animated.ValueXY.extractOffset(pan);

let moveChild = (~x=0., ~y=0., ~duration=300., ~pan, ()) =>
  Animated.(
    TimingXY.animate(
      ~value=pan,
      ~toValue=`raw({"x": x, "y": y}),
      ~duration,
      (),
    )
    |. CompositeAnimation.start(~callback=_ => resetPan(pan), ())
  );

let getWindowSize = () => {
  let rawDimension = Dimensions.get(`window);
  {
    width: float_of_int(rawDimension##width),
    height: float_of_int(rawDimension##height),
  };
};

let handleGestureType = (gesture, pan) =>
  switch (gesture) {
  | Corner({x, y}) => moveChild(~x, ~y, ~pan, ())
  | TopBottom(y) => moveChild(~y, ~pan, ())
  | LeftRight(x) => moveChild(~x, ~pan, ())
  | TouchMovement => Js.log("Touched or moved the child component")
  };

let handleRelease = (childSize, childCoordinates, pan) => {
  resetPan(pan);
  let windowSize = getWindowSize();
  let childLeft = childCoordinates##x;
  let childRight = childLeft +. childSize.width;
  let childTop = childCoordinates##y;
  let childBottom = childTop +. childSize.height;
  pan
  |> (
    switch (childLeft, childTop, childRight, childBottom) {
    | (_, t, r, _) when t < 0. && r > windowSize.width =>
      handleGestureType(Corner({x: windowSize.width -. r, y: -. t}))
    | (l, t, _, _) when l < 0. && t < 0. =>
      handleGestureType(Corner({x: -. l, y: -. t}))
    | (l, _, _, b) when l < 0. && b > windowSize.height =>
      handleGestureType(Corner({x: -. l, y: windowSize.height -. b}))
    | (_, _, r, b) when b > windowSize.height && r > windowSize.width =>
      handleGestureType(
        Corner({x: windowSize.width -. r, y: windowSize.height -. b}),
      )
    | (_, t, _, _) when t < 0. => handleGestureType(TopBottom(-. t))
    | (l, _, _, _) when l < 0. => handleGestureType(LeftRight(-. l))
    | (_, _, _, b) when b > windowSize.height =>
      handleGestureType(TopBottom(windowSize.height -. b))
    | (_, _, r, _) when r > windowSize.width =>
      handleGestureType(LeftRight(windowSize.width -. r))
    | (_, _, _, _) => handleGestureType(TouchMovement)
    }
  );
  ();
};

let component = ReasonReact.reducerComponent("GestureHandler");

let make = (~childSize: size, children) => {
  ...component,
  initialState: () => {
    let pan = Animated.ValueXY.create(~x=0., ~y=0.);
    let childCoordinates = ref({"x": 0., "y": 0.});
    {
      pan,
      panResponder:
        PanResponder.(
          create(
            ~onStartShouldSetPanResponder=callback((_e, _g) => true),
            ~onPanResponderMove=`update([`XY(pan)]),
            ~onPanResponderRelease=
              callback((_e, _g) =>
                handleRelease(childSize, childCoordinates^, pan)
              ),
            (),
          )
        ),
      childCoordinates,
      panListener:
        Animated.ValueXY.addListener(pan, raw => childCoordinates := raw),
    };
  },
  reducer: ((), _state: state) => ReasonReact.NoUpdate,
  willUnmount: ({state}) =>
    state.panListener |> Animated.ValueXY.removeListener(state.pan),
  render: ({state}) =>
    <View responderHandlers=(PanResponder.panHandlers(state.panResponder))>
      <Animated.View
        style=Style.(
                style([
                  Transform.makeAnimated(
                    ~translateX=Animated.ValueXY.getX(state.pan),
                    ~translateY=Animated.ValueXY.getY(state.pan),
                    (),
                  ),
                  position(Absolute),
                ])
              )>
        ...children
      </Animated.View>
    </View>,
};