unit RiggVar.FB.SpeedProps;

interface

uses
  System.UITypes,
  System.UIConsts,
  System.Generics.Collections,
  FMX.StdCtrls,
  RiggVar.FB.SpeedColor;

type
  TSpeedButtonProps = record
    ColorValue: TSpeedColorValue;
    IsFirstInGroup: Boolean;
  end;

  TSpeedBtnDict = TDictionary<TSpeedButton, TSpeedButtonProps>;

  ISpeedProps = interface
  ['{440319B2-3930-4DDE-9A62-DDA7E27FD1F6}']
    procedure SetSpeedButtonProps(SB: TSpeedButton; const Value: TSpeedButtonProps);
    function GetSpeedButtonProps(SB: TSpeedButton): TSpeedButtonProps;
  end;

  TSpeedProps = class(TInterfacedObject, ISpeedProps)
  private
    Storage: TSpeedBtnDict;
    DefaultValue: TSpeedButtonProps;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetSpeedButtonProps(SB: TSpeedButton; const Value: TSpeedButtonProps);
    function GetSpeedButtonProps(SB: TSpeedButton): TSpeedButtonProps;
  end;

implementation

{ TSpeedProps }

constructor TSpeedProps.Create;
begin
  Storage := TSpeedBtnDict.Create(64);
  DefaultValue.ColorValue := clvProp;
  DefaultValue.IsFirstInGroup := False;
end;

destructor TSpeedProps.Destroy;
begin
  Storage.Free;
  inherited;
end;

function TSpeedProps.GetSpeedButtonProps(SB: TSpeedButton): TSpeedButtonProps;
begin
  if not Storage.TryGetValue(SB, result) then
    result := DefaultValue;
end;

procedure TSpeedProps.SetSpeedButtonProps(SB: TSpeedButton; const Value: TSpeedButtonProps);
begin
  Storage.AddOrSetValue(SB, Value);
end;

end.
