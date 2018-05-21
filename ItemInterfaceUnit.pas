unit ItemInterfaceUnit;

interface

uses System.Types, FortranGrapherTypes, FMX.Types, FMX.Graphics;

type
  IDisplayItemList = interface;
  TItemPosition = (ipRoot, ipDescendant, ipAncestor);

  IDisplayItem = interface(IUnknown)
    function GetSize: TSize;
    function GetDescendants: IDisplayItemList;
    function GetAncestors: IDisplayItemList;
    function GetDescendantsExpanded: Boolean;
    function GetAncestorsExpanded: Boolean;
    function GetPosition: TShapePosition;
    function GetVisible: Boolean;
    function GetLineDeltaX: integer;
    function GetLineDeltaZ: integer;
    function GetItemPosition: TItemPosition;
    procedure SetPosition(Value: TShapePosition);
    procedure SetVisible(const Value: Boolean);
    procedure SetLineDeltaX(const Value: Integer);
    procedure SetLineDeltaZ(const Value: Integer);
    procedure SetItemPosition(const Value: TItemPosition);
    procedure Paint(Sender: TObject; Canvas: TCanvas);
    procedure HideChildren;
    property Size: TSize read GetSize;
    property Descendants: IDisplayItemList read GetDescendants;
    property Ancestors: IDisplayItemList read GetAncestors;
    property DescendantsExpanded: boolean read GetDescendantsExpanded;
    property AncestorsExpanded: boolean read GetAncestorsExpanded;
    property Position: TShapePosition read GetPosition write SetPosition;
    property Visible: Boolean read GetVisible write SetVisible;
    property LineDeltaX: Integer read GetLineDeltaX write SetLineDeltaX;
    property LineDeltaZ: Integer read GetLineDeltaZ write SetLineDeltaZ;
    property ItemPosition: TItemPosition read GetItemPosition write SetItemPosition;
  end;

  IDisplayItemList = interface(IUnknown)
    function GetCount: integer;
    function GetItem(Index: Integer): IDisplayItem;
    procedure SetItem(Index: Integer; Value: IDisplayItem);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IDisplayItem read GetItem write SetItem; default;
    function Add(Item: IDisplayItem): Integer;
    procedure DeleteLast;
    procedure Clear;
    function IsRecursiveItem(Item: IDisplayItem): Boolean;
  end;




implementation



end.
