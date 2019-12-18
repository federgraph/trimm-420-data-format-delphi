﻿unit RiggVar.RG.Main;

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
  Rggunit4,
  RiggVar.RG.Data;

type
  TRggMain = class
  public
    Rigg: TRigg;

    constructor Create;
    destructor Destroy; override;

    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);
  end;

implementation

{ TRggMain }

constructor TRggMain.Create;
begin
  inherited Create;
  Rigg := TRigg.Create;
end;

destructor TRggMain.Destroy;
begin
  Rigg.Free;
  inherited;
end;

procedure TRggMain.DoBigWheel(Delta: single);
begin

end;

procedure TRggMain.DoSmallWheel(Delta: single);
begin

end;

procedure TRggMain.LoadTrimm(fd: TRggData);
begin

end;

procedure TRggMain.SaveTrimm(fd: TRggData);
begin

end;

end.
