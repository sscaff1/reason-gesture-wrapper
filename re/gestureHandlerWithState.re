open BsReactNative;

type coordinates = {
  mutable x: float,
  mutable y: float,
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
  panResponder: option(PanResponder.t),
  panListener: ref(string),
};

type action =
  | SetPanResponder;

let childCoordinates = {x: 0., y: 0.};

let resetAnimatedPan = pan => Animated.ValueXY.extractOffset(pan);

let moveChild = (~x=0., ~y=0., ~duration=300., ~pan, ()) =>
  Animated.(
    TimingXY.animate(
      ~value=pan,
      ~toValue=`raw({"x": x, "y": y}),
      ~duration,
      (),
    )
    |. CompositeAnimation.start(~callback=(_) => resetAnimatedPan(pan), ())
  );

let getWindowSize = () => {
  let rawDimension = Dimensions.get(`window);
  {
    width: float_of_int(rawDimension##width),
    height: float_of_int(rawDimension##height),
  };
};

let respondToGesture = (gesture, pan) =>
  switch (gesture) {
  | Corner({x, y}) => moveChild(~x, ~y, ~pan, ())
  | TopBottom(y) => moveChild(~y, ~pan, ())
  | LeftRight(x) => moveChild(~x, ~pan, ())
  | TouchMovement => Js.log("Touched or moved the child component")
  };

let handleTouch = (childSize, pan) => {
  resetAnimatedPan(pan);
  let windowSize = getWindowSize();
  let childLeft = childCoordinates.x;
  let childRight = childLeft +. childSize.width;
  let childTop = childCoordinates.y;
  let childBottom = childTop +. childSize.height;
  pan
  |> (
    switch (childLeft, childTop, childRight, childBottom) {
    | (_, t, r, _) when t < 0. && r > windowSize.width =>
      respondToGesture(Corner({x: windowSize.width -. r, y: -. t}))
    | (l, t, _, _) when l < 0. && t < 0. =>
      respondToGesture(Corner({x: -. l, y: -. t}))
    | (l, _, _, b) when l < 0. && b > windowSize.height =>
      respondToGesture(Corner({x: -. l, y: windowSize.height -. b}))
    | (_, _, r, b) when b > windowSize.height && r > windowSize.width =>
      respondToGesture(
        Corner({x: windowSize.width -. r, y: windowSize.height -. b}),
      )
    | (_, t, _, _) when t < 0. => respondToGesture(TopBottom(-. t))
    | (l, _, _, _) when l < 0. => respondToGesture(LeftRight(-. l))
    | (_, _, _, b) when b > windowSize.height =>
      respondToGesture(TopBottom(windowSize.height -. b))
    | (_, _, r, _) when r > windowSize.width =>
      respondToGesture(LeftRight(windowSize.width -. r))
    | (_, _, _, _) => respondToGesture(TouchMovement)
    }
  );
  ();
};

let component = ReasonReact.reducerComponent("GestureHandler");

let make = (~childSize: size, children) => {
  ...component,
  initialState: () => {
    pan: Animated.ValueXY.create(~x=0., ~y=0.),
    panResponder: None,
    panListener: ref(""),
  },
  reducer: (action, state) =>
    switch (action) {
    | SetPanResponder =>
      ReasonReact.Update({
        ...state,
        panResponder:
          Some(
            PanResponder.(
              create(
                ~onStartShouldSetPanResponder=callback((_e, _g) => true),
                ~onPanResponderMove=`update([`XY(state.pan)]),
                ~onPanResponderRelease=
                  callback((_e, _g) => handleTouch(childSize, state.pan)),
                (),
              )
            ),
          ),
      })
    },
  didMount: self => {
    self.state.panListener :=
      Animated.ValueXY.addListener(
        self.state.pan,
        raw => {
          childCoordinates.x = raw##x;
          childCoordinates.y = raw##y;
        },
      );
    self.send(SetPanResponder);
  },
  willUnmount: ({state}) =>
    state.panListener^ |> Animated.ValueXY.removeListener(state.pan),
  render: ({state}) =>
    switch (state.panResponder) {
    | Some(panHandler) =>
      <View responderHandlers=(PanResponder.panHandlers(panHandler))>
        <Animated.View
          style=Style.(
                  style([
                    Transform.makeAnimated(
                      ~translateX=Animated.ValueXY.getX(state.pan),
                      ~translateY=Animated.ValueXY.getY(state.pan),
                      (),
                    ),
                  ])
                )>
          ...children
        </Animated.View>
      </View>
    | None => ReasonReact.null
    },
};