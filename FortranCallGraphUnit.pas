unit FortranCallGraphUnit;

interface

uses
  ItemInterfaceUnit, System.Types, FMX.Types, FMX.Graphics;

type
  TCallGrapher = class(TObject)
  private
    FFocusedItem: IDisplayItem;
    FVerticalBufferSize: integer;
    FHorizontalBufferSize: integer;
    FSender: TObject;
    FCanvas: TCanvas;
    FResursiveList: IDisplayItemList;
    procedure SetFocusedItem(const Value: IDisplayItem);
    procedure SetHorizontalBufferSize(const Value: integer);
    procedure SetVerticalBufferSize(const Value: integer);
    procedure PaintDescendants(Item: IDisplayItem);
    procedure PaintAncestors(Item: IDisplayItem);
  public
    constructor Create;
    function DecendentSize(Item: IDisplayItem): TSize;
    function AncestorSize(Item: IDisplayItem): TSize;
    function GraphSize: TSize;
    procedure Paint(Sender: TObject; Canvas: TCanvas);
    property FocusedItem: IDisplayItem read FFocusedItem write SetFocusedItem;
    property HorizontalBufferSize: integer read FHorizontalBufferSize
      write SetHorizontalBufferSize;
    property VerticalBufferSize: integer read FVerticalBufferSize
      write SetVerticalBufferSize;
    property ResursiveList: IDisplayItemList read FResursiveList write FResursiveList;
  end;

implementation

uses
  System.Math, FortranGrapherTypes;

{ TCallGrapher }

function TCallGrapher.DecendentSize(Item: IDisplayItem): TSize;
var
  CalledByWidth: Integer;
  CalledByHeight: Integer;
  ItemIndex: Integer;
  AnItem: IDisplayItem;
  ItemSize: TSize;
begin
  Assert(Assigned(ResursiveList));
  Result := Item.Size;
  if ResursiveList.IsRecursiveItem(Item) then
  begin
    Exit;
  end;
  if Item.DescendantsExpanded and (Item.Descendants.Count > 0) then
  begin
    ResursiveList.Add(Item);
    try
      CalledByWidth := 0;
      CalledByHeight := (Item.Descendants.Count -1) * VerticalBufferSize;
      for ItemIndex := 0 to Item.Descendants.Count - 1 do
      begin
        AnItem := Item.Descendants[ItemIndex];
        ItemSize := DecendentSize(AnItem);
        if  CalledByWidth < ItemSize.cx then
        begin
          CalledByWidth := ItemSize.cx;
        end;
        CalledByHeight := CalledByHeight + ItemSize.cy;
      end;
      Result.cx := Result.cx + CalledByWidth + HorizontalBufferSize;
      Result.cy := Max(Result.cy, CalledByHeight);
    finally
      ResursiveList.DeleteLast
    end;
  end;
end;

function TCallGrapher.AncestorSize(Item: IDisplayItem): TSize;
var
  CalledByWidth: Integer;
  CalledByHeight: Integer;
  ItemIndex: Integer;
  AnItem: IDisplayItem;
  ItemSize: TSize;
begin
  Assert(Assigned(ResursiveList));
  Result := Item.Size;
  if ResursiveList.IsRecursiveItem(Item) then
  begin
    Exit;
  end;
  if Item.AncestorsExpanded and (Item.Ancestors.Count > 0) then
  begin
    ResursiveList.Add(Item);
    try
      CalledByWidth := 0;
      CalledByHeight := (Item.Ancestors.Count -1) * VerticalBufferSize;
      for ItemIndex := 0 to Item.Ancestors.Count - 1 do
      begin
        AnItem := Item.Ancestors[ItemIndex];
        ItemSize := AncestorSize(AnItem);
        if  CalledByWidth < ItemSize.cx then
        begin
          CalledByWidth := ItemSize.cx;
        end;
        CalledByHeight := CalledByHeight + ItemSize.cy;
      end;
      Result.cx := Result.cx + CalledByWidth + HorizontalBufferSize;
      Result.cy := Max(Result.cy, CalledByHeight);
    finally
      ResursiveList.DeleteLast
    end;
  end;
end;

constructor TCallGrapher.Create;
begin
  FVerticalBufferSize := 10;
  FHorizontalBufferSize := 40;
end;

function TCallGrapher.GraphSize: TSize;
var
  ItemSize: TSize;
  FocusedSize: TSize;
begin
  Assert(Assigned(FocusedItem));
  Assert(Assigned(ResursiveList));
  ResursiveList.Clear;
//  ResursiveList.Add(FocusedItem);
  FocusedSize := FocusedItem.Size;
  Result.cx := 0;
  Result.cy := FocusedSize.cy;
//  if FocusedItem.DescendantsExpanded and (FocusedItem.Descendants.Count > 0) then
  begin
    ItemSize := DecendentSize(FocusedItem);
    Result.cx := {Result.cx +} ItemSize.cx + HorizontalBufferSize;
    Result.cy := Max(Result.cy, ItemSize.cy);
  end;
//  ResursiveList.Add(FocusedItem);
//  if FocusedItem.AncestorsExpanded and (FocusedItem.Ancestors.Count > 0) then
  begin
    ItemSize := AncestorSize(FocusedItem);
    Result.cx := Result.cx + ItemSize.cx + HorizontalBufferSize;
    Result.cy := Max(Result.cy, ItemSize.cy);
  end;
  Result.cx := Result.cx - FocusedSize.cx;
end;

procedure TCallGrapher.Paint(Sender: TObject; Canvas: TCanvas);
begin
  Assert(Assigned(FocusedItem));
  Assert(Assigned(ResursiveList));
  ResursiveList.Clear;
//  ResursiveList.Add(FocusedItem);
  FSender := Sender;
  FCanvas := Canvas;
  FocusedItem.Paint(FSender, FCanvas);
  Assert(Assigned(FocusedItem));
  PaintDescendants(FocusedItem);
//  ResursiveList.Add(FocusedItem);
  PaintAncestors(FocusedItem);
end;

procedure TCallGrapher.PaintDescendants(Item: IDisplayItem);
var
  X: integer;
  Y: integer;
  ItemIndex: Integer;
  AnItem: IDisplayItem;
  APosition: TShapePosition;
  ItemSize: TSize;
  ParentSize: TSize;
  YOffset: Integer;

begin
  if ResursiveList.IsRecursiveItem(Item) then
  begin
    Exit;
  end;
  ResursiveList.Add(Item);
  try
//  Item.Paint(FSender, FCanvas);
    if Item.DescendantsExpanded and (Item.Descendants.Count > 0) then
    begin
      ParentSize := Item.Size;
      X := Round(Item.Position.x) + (ParentSize.cx div 2) + HorizontalBufferSize;
      Y := Round(Item.Position.y + ParentSize.cy div 2);
      for ItemIndex := 0 to Item.Descendants.Count - 1 do
      begin
        AnItem := Item.Descendants[ItemIndex];

        AnItem.ItemPosition := ipDescendant;
        ItemSize := AnItem.Size;
        APosition.X := X + ItemSize.cx div 2;
        APosition.Y := Y;
        YOffset := (ParentSize.Height - ItemSize.Height) div 2;
        AnItem.LineDeltaZ := Round(Item.Position.y) - Y + (ParentSize.cy div 2) + YOffset;
        AnItem.LineDeltaX := HorizontalBufferSize;
        APosition.Y := Y - ItemSize.cy div 2 - YOffset{+ ParentSize.cy div 2};
        AnItem.Position := APosition;
        ItemSize := DecendentSize(AnItem);
        Y := Y + ItemSize.cy + VerticalBufferSize;
        AnItem.Paint(FSender, FCanvas);
        PaintDescendants(AnItem);
      end;
    end;
  finally
    ResursiveList.DeleteLast;
  end;
end;

procedure TCallGrapher.PaintAncestors(Item: IDisplayItem);
var
  X: Integer;
  Y: Integer;
  ItemIndex: Integer;
  AnItem: IDisplayItem;
  APosition: TShapePosition;
  ItemSize: TSize;
  ParentSize: TSize;
  YOffset: Integer;
begin
  if ResursiveList.IsRecursiveItem(Item) then
  begin
    Exit;
  end;
  ResursiveList.Add(Item);
  try
    if Item.AncestorsExpanded and (Item.Ancestors.Count > 0) then
    begin
      ParentSize := Item.Size;
      X := Round(Item.Position.x) - ParentSize.cx div 2 - HorizontalBufferSize;
      Y := Round(Item.Position.y + ParentSize.cy div 2);
      for ItemIndex := 0 to Item.Ancestors.Count - 1 do
      begin
        AnItem := Item.Ancestors[ItemIndex];
        AnItem.ItemPosition := ipAncestor;
        ItemSize := AnItem.Size;
        YOffset := (ParentSize.Height - ItemSize.Height) div 2;
        APosition.X := X - ItemSize.cx div 2;
        APosition.Y := Y;
        AnItem.LineDeltaZ := Round(Item.Position.y) - Y + (ParentSize.cy div 2) +
           YOffset;
        AnItem.LineDeltaX := HorizontalBufferSize;
        APosition.Y := Y - ItemSize.cy div 2 - YOffset{ + ParentSize.cy div 2};
        AnItem.Position := APosition;
        ItemSize := AncestorSize(AnItem);
        Y := Y + ItemSize.cy + VerticalBufferSize;
        AnItem.Paint(FSender, FCanvas);
        PaintAncestors(AnItem);
      end;
    end
  finally
    ResursiveList.DeleteLast;
  end;
end;

procedure TCallGrapher.SetFocusedItem(const Value: IDisplayItem);
begin
  FFocusedItem := Value;
  FFocusedItem.ItemPosition := ipRoot;
end;

procedure TCallGrapher.SetHorizontalBufferSize(const Value: integer);
begin
  FHorizontalBufferSize := Value;
end;

procedure TCallGrapher.SetVerticalBufferSize(const Value: integer);
begin
  FVerticalBufferSize := Value;
end;

end.
