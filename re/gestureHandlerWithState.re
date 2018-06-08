open BsReactNative;

type coordinates = {
  x: float,
  y: float,
};

type size = {
  width: float,
  height: float,
};

type moveDescription = (float, float, float);

type gestureTypes =
  | Corner(coordinates)
  | TopBottom(float)
  | LeftRight(float)
  | TouchMovement;

type state = {
  pan: Animated.ValueXY.t,
  panResponder: ref(option(PanResponder.t)),
  childCoordinates: coordinates,
  panListener: ref(string),
};

type action =
  | UpdateChildCoordinates(coordinates);

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

let getInitialCenterPosition = childSize => {
  let windowSize = getWindowSize();
  {
    x: (windowSize.width -. childSize.width) /. 2.,
    y: (windowSize.height -. childSize.height) /. 2.,
  };
};

let respondToGesture = (gesture, pan) =>
  switch (gesture) {
  | Corner({x, y}) => moveChild(~x, ~y, ~pan, ())
  | TopBottom(y) => moveChild(~y, ~pan, ())
  | LeftRight(x) => moveChild(~x, ~pan, ())
  | TouchMovement => Js.log("Touched or moved the child component")
  };

let handleTouch = (childSize, self) => {
  let state = self.ReasonReact.state;
  resetAnimatedPan(state.pan);
  let windowSize = getWindowSize();
  let childLeft = state.childCoordinates.x;
  let childRight = childLeft +. childSize.width;
  let childTop = state.childCoordinates.y;
  let childBottom = childTop +. childSize.height;
  state.pan
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

let make =
    (
      ~childSize: size,
      ~initialChildCoordinates=getInitialCenterPosition(childSize),
      children,
    ) => {
  ...component,
  initialState: () => {
    pan: Animated.ValueXY.create(~x=0., ~y=0.),
    panResponder: ref(None),
    childCoordinates: initialChildCoordinates,
    panListener: ref(""),
  },
  reducer: (action, state) =>
    switch (action) {
    | UpdateChildCoordinates(c) =>
      ReasonReact.Update({
        ...state,
        childCoordinates: {
          x: c.x,
          y: c.y,
        },
      })
    },
  didMount: self => {
    let {x, y} = initialChildCoordinates;
    self.state.panResponder :=
      Some(
        PanResponder.(
          create(
            ~onStartShouldSetPanResponder=callback((_e, _g) => true),
            ~onPanResponderMove=`update([`XY(self.state.pan)]),
            ~onPanResponderRelease=
              callback((_e, _g) => self.handle(handleTouch, childSize)),
            (),
          )
        ),
      );
    self.state.panListener :=
      Animated.ValueXY.addListener(self.state.pan, raw =>
        self.send(UpdateChildCoordinates({x: raw##x, y: raw##y}))
      );
    (x, y, 0.)
    |> self.handle(((x, y, duration), self) =>
         moveChild(~x, ~y, ~duration, ~pan=self.state.pan, ())
       );
    ();
  },
  willUnmount: ({state}) =>
    state.panListener^ |> Animated.ValueXY.removeListener(state.pan),
  render: ({state}) =>
    switch (state.panResponder^) {
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