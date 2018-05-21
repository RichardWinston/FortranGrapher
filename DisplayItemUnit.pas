unit DisplayItemUnit;

interface

uses ItemInterfaceUnit, System.Types, System.UIConsts, FortranCallGraphUnit,
  FMX.Objects, FortranGrapherTypes, FMX.Types, System.Generics.Collections,
  FortranReaderUnit, FMX.Controls, FMX.Graphics;

type
  TDisplaySelect = (dsNone, dsSelect, dsToggleDescendants, dsToggleAncestors);

  TDisplayItem = class(TInterfacedObject, IDisplayItem)
  private
    FDescendantsExpanded: boolean;
    FAncestorsExpanded: boolean;
    FDescendants: IDisplayItemList;
    FAncestors: IDisplayItemList;
    FItemPosition: TItemPosition;
    FLineDeltaZ: integer;
    FLineDeltaX: integer;
    FParent: TControl;
    FText: string;
    FTextRect: TRectF;
    FBoxRect: TRectF;
    FVisible: boolean;
    FPathData: TPathData;
    FPlusSign: TPathData;
    FMinusSign: TPathData;
    FDescendantControl: TRectF;
    FAncestorControl: TRectF;
    FFortranUnit: TFortranProgramUnit;
    function GetSize: TSize;
    function GetDescendants: IDisplayItemList;
    function GetAncestors: IDisplayItemList;
    function GetDescendantsExpanded: boolean;
    function GetAncestorsExpanded: boolean;
    function GetPosition: TShapePosition;
    function GetVisible: boolean;
    procedure SetPosition(Value: TShapePosition);
    procedure SetDescendantsExpanded(const Value: boolean);
    procedure SetAncestorsExpanded(const Value: boolean);
    procedure SetText(const Value: string);
    procedure SetVisible(const Value: boolean);
    function GetText: string;
    function GetLineDeltaZ: integer;
    procedure SetLineDeltaZ(const Value: integer);
    function GetLineDeltaX: integer;
    procedure SetLineDeltaX(const Value: integer);
    function GetItemPosition: TItemPosition;
    procedure SetItemPosition(const Value: TItemPosition);
    function GetFileName: string;
  public
    constructor Create(Parent: TControl; FortranUnit: TFortranProgramUnit);
    destructor Destroy; override;
    procedure HideChildren;
    procedure Paint(Sender: TObject; Canvas: TCanvas);
    property Size: TSize read GetSize;
    property Descendants: IDisplayItemList read GetDescendants;
    property Ancestors: IDisplayItemList read GetAncestors;
    property DescendantsExpanded: boolean read GetDescendantsExpanded
      write SetDescendantsExpanded;
    property AncestorsExpanded: boolean read GetAncestorsExpanded
      write SetAncestorsExpanded;
    property Position: TShapePosition read GetPosition write SetPosition;
    property Text: string read GetText write SetText;
    property Visible: boolean read GetVisible write SetVisible;
    property ItemPosition: TItemPosition read GetItemPosition
      write SetItemPosition;
    property LineDeltaX: integer read GetLineDeltaX write SetLineDeltaX;
    property LineDeltaZ: integer read GetLineDeltaZ write SetLineDeltaZ;
    procedure DeleteLinks;
    procedure HandleClick(Sender: TObject; X, Y: Single;
      var Handled: TDisplaySelect; var Item: TDisplayItem);
    function IndexName: string;
    property BoxRect: TRectF read FBoxRect;
    property FileName: string read GetFileName;
  end;

  TItemList = class(TList<IDisplayItem>);

  TPurpose = (pContained, pDescendants, pAncestors);

  TDisplayItemList = class(TInterfacedObject, IDisplayItemList)
  private
    FList: TItemList;
    FPurpose: TPurpose;
    FParentItem: TDisplayItem;
    function GetCount: integer;
    function GetItem(Index: integer): IDisplayItem;
    procedure SetItem(Index: Integer; Value: IDisplayItem);
  public
    constructor Create(ParentItem: TDisplayItem; Purpose: TPurpose);
    destructor Destroy; override;
    function Add(Item: IDisplayItem): integer;
    property Count: integer read GetCount;
    property Items[Index: integer]: IDisplayItem read GetItem; default;
    procedure DeleteLast;
    procedure Clear;
    function IsRecursiveItem(Item: IDisplayItem): Boolean;
  end;

implementation

uses
  System.Math, System.Classes, System.UITypes, frmMainFMUnit;

{ TDisplayItem }

constructor TDisplayItem.Create(Parent: TControl; FortranUnit: TFortranProgramUnit);
const
  SymbolWidth = 9;
  SymbolCenter = 5;
var
  APoint: TPointF;
begin
  FParent := Parent;
  FFortranUnit := FortranUnit;

  FPathData := TPathData.Create;
  APoint.X := 0;
  APoint.Y := 0;
  FPathData.MoveTo(APoint);
  APoint.X := 1;
  FPathData.LineTo(APoint);
  APoint.X := 3;
  APoint.Y := 1;
  FPathData.LineTo(APoint);
  APoint.X := 4;
  FPathData.LineTo(APoint);

  FMinusSign := TPathData.Create;
  APoint.X := 0;
  APoint.Y := 0;
  FMinusSign.MoveTo(APoint);
  APoint.X := SymbolWidth;
  FMinusSign.LineTo(APoint);
  APoint.Y := SymbolWidth;
  FMinusSign.LineTo(APoint);
  APoint.X := 0;
  FMinusSign.LineTo(APoint);
  APoint.Y := 0;
  FMinusSign.LineTo(APoint);

  APoint.X := 2;
  APoint.Y := SymbolCenter;
  FMinusSign.MoveTo(APoint);
  APoint.X := SymbolWidth - 2;
  FMinusSign.LineTo(APoint);

  FPlusSign := TPathData.Create;
  FPlusSign.Assign(FMinusSign);
  APoint.X := SymbolCenter;
  APoint.Y := 2;
  FPlusSign.MoveTo(APoint);
  APoint.Y := SymbolWidth - 2;
  FPlusSign.LineTo(APoint);

  FDescendants := TDisplayItemList.Create(self, pDescendants);
  FAncestors := TDisplayItemList.Create(self, pAncestors);

  AncestorsExpanded := True;
  DescendantsExpanded := True;
end;

procedure TDisplayItem.DeleteLinks;
var
  ItemIndex: integer;
  Temp: TDisplayItemList;
  AnItem: TDisplayItem;
  TempI: IDisplayItemList;
begin
  if Assigned(FDescendants) then
  begin
    TempI := FDescendants;
    Temp := FDescendants as TDisplayItemList;
    FDescendants := nil;
    for ItemIndex := 0 to Temp.FList.Count - 1 do
    begin
      AnItem := Temp.FList[ItemIndex] as TDisplayItem;
      if Assigned(AnItem) then
      begin
        AnItem.DeleteLinks;
      end;
    end;
  end;
  if Assigned(FAncestors) then
  begin
    TempI := FAncestors;
    Temp := FAncestors as TDisplayItemList;
    FAncestors := nil;
    for ItemIndex := 0 to Temp.FList.Count - 1 do
    begin
      AnItem := Temp.FList[ItemIndex] as TDisplayItem;
      if Assigned(AnItem) then
      begin
        AnItem.DeleteLinks;
      end;
    end;
  end;
end;

destructor TDisplayItem.Destroy;
begin
  FPathData.Free;
  FPlusSign.Free;
  FMinusSign.Free;
  inherited;
end;

function TDisplayItem.GetDescendants: IDisplayItemList;
begin
  result := FDescendants;
end;

function TDisplayItem.GetDescendantsExpanded: boolean;
begin
  result := FDescendantsExpanded;
end;

function TDisplayItem.GetFileName: string;
begin
  result := FFortranUnit.FileName;
end;

function TDisplayItem.GetItemPosition: TItemPosition;
begin
  result := FItemPosition;
end;

function TDisplayItem.GetLineDeltaX: integer;
begin
  result := FLineDeltaX;
end;

function TDisplayItem.GetLineDeltaZ: integer;
begin
  result := FLineDeltaZ;
end;

function TDisplayItem.GetAncestors: IDisplayItemList;
begin
  result := FAncestors;
end;

function TDisplayItem.GetAncestorsExpanded: boolean;
begin
  result := FAncestorsExpanded;
end;

function TDisplayItem.GetPosition: TShapePosition;
begin
  result := FBoxRect.TopLeft;
  result.X := result.X + FBoxRect.Width / 2;
end;

function TDisplayItem.GetSize: TSize;
begin
  Text;
  result.cx := Ceil(FBoxRect.Width);
  result.cy := Ceil(FBoxRect.Height);
end;

function TDisplayItem.GetText: string;
begin
  if Assigned(FFortranUnit) then
  begin
    result := FFortranUnit.DisplayName;
    if FText <> result then
    begin
      Text := result;
    end;
  end
  else
  begin
    result := FText;
  end;
end;

function TDisplayItem.GetVisible: boolean;
begin
  result := FVisible;
end;

procedure TDisplayItem.HandleClick(Sender: TObject; X, Y: Single;
  var Handled: TDisplaySelect; var Item: TDisplayItem);
var
  ItemIndex: integer;
  AnItem: IDisplayItem;
begin
  Handled := dsNone;
  Item := nil;
  if FBoxRect.Contains(PointF(X, Y)) then
  begin
    Handled := dsSelect;
    Item := Self;
    Exit;
  end;
  if (ItemPosition in [ipRoot, ipDescendant]) and (Descendants.Count > 0) then
  begin
    if FDescendantControl.Contains(PointF(X, Y)) then
    begin
      DescendantsExpanded := not DescendantsExpanded;
      Handled := dsToggleDescendants;
      Item := Self;
      Exit;
    end;
    if DescendantsExpanded and (X > FBoxRect.Right) then
    begin
      for ItemIndex := 0 to Descendants.Count - 1 do
      begin
        AnItem := Descendants[ItemIndex];
        (AnItem as TDisplayItem).HandleClick(Sender, X, Y, Handled, Item);
        if Handled <> dsNone then
        begin
          Exit;
        end;
      end;
    end;
  end;
  if (ItemPosition in [ipRoot, ipAncestor]) and (Ancestors.Count > 0) then
  begin
    if FAncestorControl.Contains(PointF(X, Y)) then
    begin
      AncestorsExpanded := not AncestorsExpanded;
      Handled := dsToggleAncestors;
      Item := Self;
      Exit;
    end;
    if AncestorsExpanded and (X < FBoxRect.Left) then
    begin
      for ItemIndex := 0 to Ancestors.Count - 1 do
      begin
        AnItem := Ancestors[ItemIndex];
        (AnItem as TDisplayItem).HandleClick(Sender, X, Y, Handled, Item);
        if Handled <> dsNone then
        begin
          Exit;
        end;
      end;
    end;
  end;

end;

procedure TDisplayItem.HideChildren;
var
  ItemIndex: integer;
  AnItem: IDisplayItem;
begin
  for ItemIndex := 0 to Descendants.Count - 1 do
  begin
    AnItem := Descendants[ItemIndex];
    AnItem.Visible := False;
    AnItem.HideChildren;
  end;
end;

function TDisplayItem.IndexName: string;
begin
  result := FFortranUnit.IndexName;
end;

procedure TDisplayItem.Paint(Sender: TObject; Canvas: TCanvas);
const
  XRadius = 8;
  ControlOffset = 3;
var
  APoint1: TPointF;
  B: TRectF;
  PathRect: TRectF;
  LocalPathData: TPathData;
  APoint2: TPointF;
begin

  Canvas.Fill.Color := claWhite;
  Canvas.Fill.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Color := claBlack;

  Canvas.DrawRect(FBoxRect, XRadius, XRadius, AllCorners, 1);

  Canvas.Fill.Color := claBlack;
  Canvas.Fill.Kind := TBrushKind.bkSolid;

  Canvas.Font.Size := 18;
  Canvas.Font.Family := 'Times New Roman';
  if ItemPosition = ipRoot then
  begin
    Canvas.Font.Style := [TFontStyle.fsBold];
//    Canvas.Font.Style := [];
  end
  else
  begin
    Canvas.Font.Style := [];
  end;
  Canvas.Fill.Color := claBlack;
  Canvas.Fill.Kind := TBrushKind.bkSolid;

  Canvas.FillText(FTextRect, FText, True, 1, [], TTextAlign.taCenter,
    TTextAlign.taCenter);

  LocalPathData := TPathData.Create;
  try
    case ItemPosition of
      ipRoot:
        begin
        end;
      ipDescendant:
        begin
          APoint1.X := FBoxRect.Left - LineDeltaX;
          APoint1.Y := FBoxRect.Height / 2.0 + FBoxRect.Top + LineDeltaZ;
          if LineDeltaZ = 0 then
          begin
            APoint2.X := FBoxRect.Left;
            APoint2.Y := APoint1.Y;

            Canvas.DrawLine(APoint1, APoint2, 1);
          end
          else
          begin
            PathRect.TopLeft := APoint1;
            PathRect.Width := LineDeltaX;
            PathRect.Height := -LineDeltaZ;

            LocalPathData.Clear;
            LocalPathData.Assign(FPathData);
            Assert(not LocalPathData.IsEmpty);

            B := LocalPathData.GetBounds;
            LocalPathData.Translate(-B.Left, -B.Top);
            LocalPathData.Scale(RectWidth(PathRect) / RectWidth(B),
              RectHeight(PathRect) / RectHeight(B));
            LocalPathData.Translate(PathRect.Left, PathRect.Top);

            Canvas.DrawPath(LocalPathData, 1);
          end;
        end;
      ipAncestor:
        begin
          APoint1.X := FBoxRect.Right;
          APoint1.Y := FBoxRect.Height / 2.0 + FBoxRect.Top + LineDeltaZ;

          if LineDeltaZ = 0 then
          begin
            APoint2.X := FBoxRect.Right + LineDeltaX;
            APoint2.Y := APoint1.Y;

            Canvas.DrawLine(APoint1, APoint2, 1);
          end
          else
          begin
            PathRect.TopLeft := APoint1;
            PathRect.Width := LineDeltaX;
            PathRect.Height := -LineDeltaZ;

            LocalPathData.Clear;
            LocalPathData.Assign(FPathData);
            Assert(not LocalPathData.IsEmpty);

            B := LocalPathData.GetBounds;
            LocalPathData.Translate(-B.Left, -B.Top);
            LocalPathData.Scale(1, -1);
            B := LocalPathData.GetBounds;
            LocalPathData.Translate(-B.Left, -B.Top);
            LocalPathData.Scale(RectWidth(PathRect) / RectWidth(B),
              RectHeight(PathRect) / RectHeight(B));
            LocalPathData.Translate(PathRect.Left, PathRect.Top);

            Canvas.DrawPath(LocalPathData, 1);

          end;
        end;
    else
      Assert(False);
    end;

    if (ItemPosition in [ipRoot, ipDescendant]) and (Descendants.Count > 0) then
    begin
      LocalPathData.Clear;
      if DescendantsExpanded then
      begin
        LocalPathData.Assign(FMinusSign);
      end
      else
      begin
        LocalPathData.Assign(FPlusSign);
      end;
      B := LocalPathData.GetBounds;

      APoint1.X := FBoxRect.Right + ControlOffset;
      APoint1.Y := FBoxRect.Top + FBoxRect.Height / 2 - B.Height -
        ControlOffset;
      B.Offset(APoint1);

      Canvas.Fill.Color := claWhite;
      Canvas.Fill.Kind := TBrushKind.bkSolid;
      Canvas.FillRect(B, 0, 0, AllCorners, 1);

      Canvas.Stroke.Kind := TBrushKind.bkSolid;
      Canvas.Stroke.Color := claBlack;

      LocalPathData.Translate(B.Left, B.Top);
      Canvas.DrawPath(LocalPathData, 1);
      FDescendantControl := B;
    end;

    if (ItemPosition in [ipRoot, ipAncestor]) and (Ancestors.Count > 0) then
    begin
      LocalPathData.Clear;
      if AncestorsExpanded then
      begin
        LocalPathData.Assign(FMinusSign);
      end
      else
      begin
        LocalPathData.Assign(FPlusSign);
      end;
      B := LocalPathData.GetBounds;

      APoint1.X := FBoxRect.Left - B.Width - ControlOffset;
      APoint1.Y := FBoxRect.Top + FBoxRect.Height / 2 - B.Height -
        ControlOffset;
      B.Offset(APoint1);

      Canvas.Fill.Color := claWhite;
      Canvas.Fill.Kind := TBrushKind.bkSolid;
      Canvas.FillRect(B, 0, 0, AllCorners, 1);

      Canvas.Stroke.Kind := TBrushKind.bkSolid;
      Canvas.Stroke.Color := claBlack;

      LocalPathData.Translate(B.Left, B.Top);
      Canvas.DrawPath(LocalPathData, 1);
      FAncestorControl := B;
    end;

  finally
    LocalPathData.Free;
  end;

end;

procedure TDisplayItem.SetDescendantsExpanded(const Value: boolean);
begin
  FDescendantsExpanded := Value;
end;

procedure TDisplayItem.SetAncestorsExpanded(const Value: boolean);
begin
  FAncestorsExpanded := Value;
end;

procedure TDisplayItem.SetItemPosition(const Value: TItemPosition);
var
  ChangedPosition: Boolean;
begin
  ChangedPosition :=  FItemPosition <> Value;
  FItemPosition := Value;
  if ChangedPosition then
  begin
    Text;
  end;
end;

procedure TDisplayItem.SetLineDeltaX(const Value: integer);
begin
  FLineDeltaX := Value;
end;

procedure TDisplayItem.SetLineDeltaZ(const Value: integer);
begin
  FLineDeltaZ := Value;
end;

procedure TDisplayItem.SetPosition(Value: TShapePosition);
var
  APoint: TPointF;
begin
  FBoxRect.Offset(-FBoxRect.Left, -FBoxRect.Top);
  Value.X := Value.X - FBoxRect.Width / 2;
  FBoxRect.Offset(Value);

  FTextRect.Offset(-FTextRect.Left, -FTextRect.Top);
  FTextRect.Offset(FBoxRect.TopLeft);
  APoint.X := (FBoxRect.Width - FTextRect.Width) / 2;
  APoint.Y := (FBoxRect.Height - FTextRect.Height) / 2;
  FTextRect.Offset(APoint);

end;

procedure TDisplayItem.SetText(const Value: string);
const
  XRadius = 8;
var
  Splitter: TStringList;
  Index: integer;
  AWord: string;
  TextWidth: Single;
  Canvas: TCanvas;
  R: TRectF;
  APosition: TShapePosition;
begin
  FText := Value;
  R := RectF(0, 0, 4, 4);
  if FText <> '' then
  begin
    TextWidth := 0;
    Splitter := TStringList.Create;
    Canvas := FParent.Canvas;
    try
      Canvas.Font.Size := 18;
      Canvas.Font.Family := 'Times New Roman';
      Canvas.Font.Style := [TFontStyle.fsBold];
      Splitter.Delimiter := ' ';
      Splitter.DelimitedText := Value;
      for index := 0 to Splitter.Count - 1 do
      begin
        AWord := Splitter[index];
        R := RectF(0, 0, MaxSingle, MaxSingle);
        Canvas.MeasureText(R, AWord, False, [], TTextAlign.taLeading,
          TTextAlign.taLeading);
        if R.Width > TextWidth then
        begin
          TextWidth := R.Width;
        end;
      end;
      R := RectF(0, 0, TextWidth + 4, MaxSingle);
      Canvas.MeasureText(R, FText, True, [], TTextAlign.taLeading,
        TTextAlign.taLeading);
      R.Right := R.Right + 4;
      R.Bottom := R.Bottom + Splitter.Count * 4;
    finally
      Splitter.Free;
    end;
    FTextRect := R;

    R := FTextRect;
    R.Offset(-R.Left, -R.Top);
    R.Right := FTextRect.Width + 2 * XRadius + 10;
    R.Bottom := FTextRect.Height + 4;
    APosition := Self.Position;
    APosition.X := -R.Width / 2 + FTextRect.Width / 2;
    APosition.Y := -2;
    R.Offset(APosition);
    R.Offset(Self.Position);
    FBoxRect := R;

  end;
end;

procedure TDisplayItem.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

{ TDisplayItemList }

function TDisplayItemList.Add(Item: IDisplayItem): integer;
begin
  result := FList.Add(Item);
end;

procedure TDisplayItemList.Clear;
begin
  FList.Clear;
end;

constructor TDisplayItemList.Create(ParentItem: TDisplayItem;
  Purpose: TPurpose);
begin
  FList := TItemList.Create;
  FParentItem := ParentItem;
  FPurpose := Purpose;
  case FPurpose of
    pContained: ;  // do nothing
    pDescendants, pAncestors:
    begin
      Assert(Assigned(FParentItem));
    end;
  end;
end;

procedure TDisplayItemList.DeleteLast;
begin
  FList.Delete(FList.Count-1);
end;

destructor TDisplayItemList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TDisplayItemList.GetCount: integer;
begin
  result := 0;
  case FPurpose of
    pContained: result := FList.Count;
    pDescendants: result := FParentItem.FFortranUnit.Calls.Count;
    pAncestors: result := FParentItem.FFortranUnit.IsCalledBy.Count;
    else Assert(False);
  end;

end;

function TDisplayItemList.GetItem(Index: integer): IDisplayItem;
var
  ChildI: IDisplayItem;
  ParentPosition: Integer;
begin
  Assert((Index >= 0) and (Index < Count));
  While Index >= FList.Count do
  begin
    FList.Add(nil);
  end;
  result := FList[Index];
  case FPurpose of
    pContained: ; // do nothing
    pDescendants:
      begin
        if result = nil then
        begin
          result := TDisplayItem.Create(FParentItem.FParent,
            FParentItem.FFortranUnit.Calls[Index]);
          FList[Index] := result;
          ChildI := result;
//          frmMainFM.DisplayItemList.Add(result);
          result.ItemPosition := ipDescendant;
          FParentItem.Descendants[Index] := result;
          ParentPosition := (result as TDisplayItem).FFortranUnit.IsCalledBy.
            IndexOf(FParentItem.FFortranUnit);
          result.Ancestors[ParentPosition] := FParentItem;
//          DescendantsExpanded := True;
        end;
      end;
    pAncestors:
      begin
        if result = nil then
        begin
          result := TDisplayItem.Create(FParentItem.FParent,
            FParentItem.FFortranUnit.IsCalledBy[Index]);
          FList[Index] := result;
          ChildI := result;
//          frmMainFM.DisplayItemList.Add(result);
          result.ItemPosition := ipAncestor;
          FParentItem.Ancestors[Index] := result;
          ParentPosition := (result as TDisplayItem).FFortranUnit.Calls.
            IndexOf(FParentItem.FFortranUnit);
          result.Descendants[ParentPosition] := FParentItem;
        end;
      end;
    else
      Assert(False);
  end;

end;

function TDisplayItemList.IsRecursiveItem(Item: IDisplayItem): Boolean;
var
  SearchItem: TDisplayItem;
  index: Integer;
  AnItem: TDisplayItem;
begin
  SearchItem := Item as TDisplayItem;
  result := False;
  for index := 0 to FList.Count - 1 do
  begin
    AnItem := FList[index] as TDisplayItem;
    Result := SearchItem.FFortranUnit = AnItem.FFortranUnit;
    if Result then
    begin
      Exit;
    end;
  end;
end;

procedure TDisplayItemList.SetItem(Index: Integer; Value: IDisplayItem);
begin
  Assert((Index >= 0) and (Index < Count));
  While Index >= FList.Count do
  begin
    FList.Add(nil);
  end;
  FList[Index] := Value;
end;


end.
