unit RiggVar.App.Main0;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.FB.ActionConst,
  RiggVar.Util.Logger;

type
  TMain0 = class
  protected
    FL: TStringList;
    procedure CopyText;
  public
    IsUp: Boolean;

    Logger: TLogger;

    constructor Create;
    destructor Destroy; override;

    procedure ExecuteAction(fa: Integer); virtual;
    function GetChecked(fa: TFederAction): Boolean; virtual;

    procedure DoTouchbarLeft(Delta: single);
    procedure DoTouchbarRight(Delta: single);
    procedure DoTouchbarBottom(Delta: single);
    procedure DoTouchbarTop(Delta: single);

    procedure PlusOne;
    procedure PlusTen;
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);
  end;

implementation

uses
  FrmMain,
  FMX.Platform,
  RiggVar.App.Main;

{ TMain0 }

constructor TMain0.Create;
begin
  FL := TStringList.Create;
  Logger := TLogger.Create;
end;

destructor TMain0.Destroy;
begin
  Logger.Free;
  FL.Free;
  inherited;
end;

procedure TMain0.CopyText;
var
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    cbs.SetClipboard(FL.Text);
    Logger.Info('in CopyText ( check clipboard )');
  end;
end;

procedure TMain0.ExecuteAction(fa: Integer);
begin
  if IsUp then
  case fa of
    faNoop: ;

    else
      FormMain.HandleAction(fa);
  end;
end;

function TMain0.GetChecked(fa: TFederAction): Boolean;
begin
  result := false;
end;

procedure TMain0.DoTouchbarLeft(Delta: single);
begin
//  DoMouseWheel([ssCtrl], Round(Delta));
end;

procedure TMain0.DoTouchbarTop(Delta: single);
begin
//  FormMain.RotaForm.RotateZ(Delta * 0.3);
end;

procedure TMain0.DoTouchbarRight(Delta: single);
begin
  DoMouseWheel([ssShift], Round(Delta));
end;

procedure TMain0.DoTouchbarBottom(Delta: single);
begin
//  FormMain.RotaForm.Zoom(Delta);
end;

procedure TMain0.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);
begin
  if ssCtrl in Shift then
  begin
    Main.DoBigWheel(WheelDelta);
  end
  else if ssShift in Shift then
  begin
    Main.DoSmallWheel(WheelDelta);
  end
end;

procedure TMain0.PlusOne;
begin
  DoMouseWheel([ssShift], 1);
end;

procedure TMain0.PlusTen;
begin
  DoMouseWheel([ssCtrl], 1);
end;

end.
