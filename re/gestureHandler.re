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

let childCoordinates = {x: 0., y: 0.};

let childSize = ref({width: 0., height: 0.});

let setChildSize = size => childSize := size;

let getWindowSize = () => {
  let rawDimension = Dimensions.get(`window);
  {
    width: float_of_int(rawDimension##width),
    height: float_of_int(rawDimension##height),
  };
};

let pan = Animated.ValueXY.create(~x=0., ~y=0.);

let panListenerCallBack = rawValue => {
  childCoordinates.x = rawValue##x;
  childCoordinates.y = rawValue##y;
};

let panListener = Animated.ValueXY.addListener(pan, panListenerCallBack);

let resetPan = (_) => Animated.ValueXY.extractOffset(pan);

let moveChild = (~x=0., ~y=0., ()) =>
  Animated.(
    TimingXY.animate(
      ~value=pan,
      ~toValue=`raw({"x": x, "y": y}),
      ~duration=300.,
      (),
    )
    |. CompositeAnimation.start(~callback=resetPan, ())
  );

let handleGestureType =
  fun
  | Corner({x, y}) => moveChild(~x, ~y, ())
  | TopBottom(y) => moveChild(~y, ())
  | LeftRight(x) => moveChild(~x, ())
  | TouchMovement => Js.log("no automated move");

let handleRelease = (_e, _g) => {
  resetPan(pan);
  let windowSize = getWindowSize();
  let childLeft = childCoordinates.x;
  let childRight = childLeft +. childSize^.width;
  let childTop = childCoordinates.y;
  let childBottom = childTop +. childSize^.height;
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
  };
};

let panResponder =
  PanResponder.(
    create(
      ~onStartShouldSetPanResponder=callback((_e, _g) => true),
      ~onPanResponderMove=`update([`XY(pan)]),
      ~onPanResponderRelease=callback(handleRelease),
      (),
    )
  );

let panHandlers = PanResponder.panHandlers(panResponder);

let component = ReasonReact.statelessComponent("GestureHandler");

let make = (~childSize, children) => {
  ...component,
  didMount: _self => setChildSize(childSize),
  render: _self =>
    <View responderHandlers=panHandlers>
      <Animated.View
        style=Style.(
                style([
                  Transform.makeAnimated(
                    ~translateX=Animated.ValueXY.getX(pan),
                    ~translateY=Animated.ValueXY.getY(pan),
                    (),
                  ),
                ])
              )>
        ...children
      </Animated.View>
    </View>,
};