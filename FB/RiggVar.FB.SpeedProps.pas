unit RiggVar.FB.SpeedProps;

interface

uses
  FMX.StdCtrls,
  RiggVar.FB.SpeedColor;

type
  TSpeedBtn = class(TSpeedButton)
  public
    ColorValue: TSpeedColorValue;
    IsFirstInGroup: Boolean;
  end;

implementation

end.
