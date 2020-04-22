unit RiggVar.FB.SpeedColor;

interface

uses
  System.UITypes,
  System.UIConsts;

type
  TSpeedColorValue = (
    clvBack,
    clvHot,
    clvLog,
    clvReport,
    clvOption,
    clvProp,
    clvData,
    clvWheel,
    clvTrimm
  );

  TSpeedColorScheme = record
    claBack: TAlphaColor;
    claHot: TAlphaColor;
    claLog: TAlphaColor;
    claReport: TAlphaColor;
    claOption: TAlphaColor;
    claProp: TAlphaColor;
    claData: TAlphaColor;
    claWheel: TAlphaColor;
    claTrimm: TAlphaColor;
    procedure InitDark;
    procedure InitLight;
    function GetColor(Value: TSpeedColorValue): TAlphaColor;
  end;

implementation

{ TSpeedColorScheme }

function TSpeedColorScheme.GetColor(Value: TSpeedColorValue): TAlphaColor;
begin
  case Value of
    clvBack: result := claBack;
    clvHot: result := claHot;
    clvLog: result := claLog;
    clvReport: result := claReport;
    clvOption: result := claOption;
    clvProp: result := claProp;
    clvData: result := claData;
    clvWheel: result := claWheel;
    clvTrimm: result := claTrimm;
    else
      result := claRed;
  end;
end;

procedure TSpeedColorScheme.InitDark;
begin
  claBack := claSlateGray;
  claHot := claBeige;
  claLog := claOrange;
  claReport := claOrange;
  claOption := claOrangeRed;
  claProp := claGoldenrod;
  claData := claLime;
  claWheel := claAqua;
  claTrimm := claYellow;
end;

procedure TSpeedColorScheme.InitLight;
begin
  claBack := StringToAlphaColor('#FFF0F0F0'); // Anti-Flash White
  claHot := claBlack;
  claLog := claOrange;
  claReport := claBurlywood;
  claOption := claOrangeRed;
  claProp := claGoldenrod;
  claData := claGreen;
  claWheel := claCoral;
  claTrimm := claDodgerblue;
end;

end.
