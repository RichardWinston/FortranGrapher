unit frmMainFMUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Edit,
  FMX.Layouts, FMX.Memo, ItemInterfaceUnit, DisplayItemUnit, FMX.TreeView,
  FMX.Menus, FortranReaderUnit, {WebAdapt, WebComp,} Strset, FMX.StdCtrls,
  FMX.Graphics, FMX.SpinBox, FMX.EditBox, FMX.Controls.Presentation;

type
  TFortranTreeViewItem = class(TTreeViewItem)
  private
    FFortranItem: TFortranProgramUnit;
    FDisplayItem: IDisplayItem;
  public
    property FortranItem: TFortranProgramUnit read FFortranItem
      write FFortranItem;
    property DisplayItem: IDisplayItem read FDisplayItem write FDisplayItem;
  end;

  TfrmMainFM = class(TForm)
    scrlbx1: TScrollBox;
    pnl1: TPanel;
    btnSaveImage: TButton;
    sd1: TSaveDialog;
    pb1: TPaintBox;
    dlgOpen: TOpenDialog;
    mnbr1: TMenuBar;
    miFile: TMenuItem;
    miOpen: TMenuItem;
    tv1: TTreeView;
    spl1: TSplitter;
    spnbxExpandLimit: TSpinBox;
    cbShowFileName: TCheckBox;
    btnSaveWebSite: TButton;
    lblDepth: TLabel;
    edTitle: TEdit;
    strstWeb1: TStrSet;
    strstWeb2: TStrSet;
    strstWeb3: TStrSet;
    strstWebIndex1: TStrSet;
    strstWebIndex2: TStrSet;
    strstWebIndex3: TStrSet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveImageClick(Sender: TObject);
    procedure pb1Paint(Sender: TObject; Canvas: TCanvas);
    procedure pb1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure miOpenClick(Sender: TObject);
    procedure tv1Change(Sender: TObject);
    procedure spnbxExpandLimitChange(Sender: TObject);
    procedure cbShowFileNameChange(Sender: TObject);
    procedure btnSaveWebSiteClick(Sender: TObject);
  private
    // FDisplayItemList: IDisplayItemList;
    FRoot: TDisplayItem;
    FDictionary: TFortranUnitDictionary;
    procedure SetRoot(const Value: TDisplayItem);
    procedure WriteImageMap(Lines: TStrings);
    procedure WriteWebPage(Lines: TStrings; WebFileName: string; Selected: TFortranTreeViewItem);
    procedure HandleChildrenTOC(Item: TFortranTreeViewItem; Indent: Integer;
      const FileName: string; Lines: TStrings);
    procedure ClearTreeView;
    property Root: TDisplayItem read FRoot write SetRoot;
    procedure UpdatePaintBoxSize;
    procedure SaveBitmap(AFileName: TFileName);
    procedure SaveWebPage(Selected: TFortranTreeViewItem; FileName: TFileName);
    { Private declarations }
  public
    // property DisplayItemList: IDisplayItemList read FDisplayItemList;
    { Public declarations }
  end;

var
  frmMainFM: TfrmMainFM;

implementation

uses
    Vcl.FileCtrl, FortranCallGraphUnit, FortranGrapherTypes,
  System.Generics.Collections;

{$R *.fmx}

procedure TfrmMainFM.WriteImageMap(Lines: TStrings);
var
  MapName: string;
  ALine: string;
  ParentFile: string;
  IList: IDisplayItemList;
  procedure WriteImageMapRect(Item: TDisplayItem);
  const
    LineF = '  <area shape="rect" coords="%0:d,%1:d,%2:d,%3:d"'
      + ' href="%4:s" '
      + 'alt="%5:s" '
      + 'title="%5:s" />';
  var
    ItemRect: TRectF;
    FileName: string;
    ItemName: string;
    URL: string;
    Anchor: string;
  begin
    ItemRect := Item.BoxRect;
    FileName := ChangeFileExt(Item.FileName, '.html');
    ItemName := Item.IndexName;
    Anchor := StringReplace(ItemName, ' ', '_', [rfReplaceAll]);
    if FileName = ParentFile then
    begin
      URL := '#' + Anchor;
    end
    else
    begin
      URL := FileName + '#' + Anchor;
    end;
    ALine := Format(LineF, [Round(ItemRect.Left), Round(ItemRect.Top),
      Round(ItemRect.Right), Round(ItemRect.Bottom), URL, ItemName]);
    Lines.Add(ALine);
  end;
  procedure HandleDescendants(Item: TDisplayItem);
  var
    ItemIndex: Integer;
    ChildItem: TDisplayItem;
  begin
    if Item <> Root then
    begin
      WriteImageMapRect(Item);
    end;
    if IList.IsRecursiveItem(Item) then
    begin
      Exit;
    end;
    IList.Add(Item);
    try
      if Item.DescendantsExpanded then
      begin
        for ItemIndex := 0 to Item.Descendants.Count - 1 do
        begin
          ChildItem := Item.Descendants[ItemIndex] as TDisplayItem;
          HandleDescendants(ChildItem);
        end;
      end;
    finally
      IList.DeleteLast;
    end;
  end;
  procedure HandleAncestors(Item: TDisplayItem);
  var
    ItemIndex: Integer;
    ChildItem: TDisplayItem;
  begin
    if Item <> Root then
    begin
      WriteImageMapRect(Item);
    end;
    if IList.IsRecursiveItem(Item) then
    begin
      Exit;
    end;
    IList.Add(Item);
    try
      if Item.AncestorsExpanded then
      begin
        for ItemIndex := 0 to Item.Ancestors.Count - 1 do
        begin
          ChildItem := Item.Ancestors[ItemIndex] as TDisplayItem;
          HandleAncestors(ChildItem);
        end;
      end;
    finally
      IList.DeleteLast;
    end;
  end;

begin
  if Assigned(Root) then
  begin
//    Lines.Clear;
    IList := TDisplayItemList.Create(nil, pContained);

    MapName := StringReplace(Root.IndexName, ' ', '_', [rfReplaceAll]);

    ALine := '<map name="' + MapName + '">';
    Lines.Add(ALine);
    ParentFile := ChangeFileExt(Root.FileName, '.html');

    HandleDescendants(Root);
    HandleAncestors(Root);

    ALine := '</map>';
    Lines.Add(ALine);
  end;
end;

procedure TfrmMainFM.HandleChildrenTOC(Item: TFortranTreeViewItem;
  Indent: Integer; const FileName: string; Lines: TStrings);
const
  TitleS2 = '      <li><a href="%1:s#%0:s">%0:s</a>';
var
  ChildIndex: Integer;
  AChild: TFortranTreeViewItem;
  ALine: string;
  Index: Integer;
begin
  ALine := '';
  for Index := 0 to Indent - 1 do
  begin
    ALine := ALine + ' ';
  end;
  if Indent <> 0 then
  begin
    Lines.Add(ALine + Format(TitleS2, [Item.Text, FileName]));
  end;
  if Item.Count > 0 then
  begin
    Lines.Add(ALine + '    <ol>');
  end;
  for ChildIndex := 0 to Item.Count - 1 do
  begin
    AChild := Item.Items[ChildIndex] as TFortranTreeViewItem;
    HandleChildrenTOC(AChild, Indent + 2, FileName, Lines)
  end;
  if Item.Count > 0 then
  begin
    Lines.Add(ALine + '    </ol>');
  end;
end;


procedure TfrmMainFM.WriteWebPage(Lines: TStrings; WebFileName: string; Selected: TFortranTreeViewItem);
const
  TitleS = '    <title>%s</title>';
  HeadS = '    <h1>%s</h1>';
var
  Title: string;
  WebDir: string;
  procedure HandleChildren(Item: TFortranTreeViewItem; Indent, HLevel: Integer);
  const
    TitleS3 = '    <h%0:d><a name="%1:s"></a>%1:s</h%0:d>';
    ImageLineS = '    <p><img alt="%0:s" src="%1:s"  usemap="#%2:s" height="%3:d" width="%4:d"/><br>';
  var
    ChildIndex: Integer;
    AChild: TFortranTreeViewItem;
    ALine: string;
    ImageName: string;
    Index: Integer;
    Limit: Integer;
    SavedShowFileName: Boolean;
    SavedDepth: Extended;
    MapName: string;
    CommentIndex: Integer;
  begin
    ALine := '';
    for Index := 0 to Indent - 1 do
    begin
      ALine := ALine + ' ';
    end;

    Lines.Add(Format(TitleS3, [HLevel, Item.Text]));
    if Assigned(Item.FFortranItem) and
      (Item.FFortranItem.Kind in [fbkProgram, fbkSubroutine, fbkFunction,
      fbkModule, fbkBlockData]) then
    begin
      if (Item.FortranItem.CommentLines.Count > 0) then
      begin
        Lines.Add('<pre>');
        for CommentIndex := 0 to Item.FortranItem.CommentLines.Count - 1 do
        begin
          Lines.Add(Item.FortranItem.CommentLines[CommentIndex]);
        end;
        Lines.Add('</pre>');
      end;
      tv1.Selected := Item;
      tv1Change(nil);
//      pb1Paint(pb1, pb1.Canvas);
      ImageName := StringReplace(ChangeFileExt(Item.Text, '.png'), ' ', '_',
        [rfReplaceAll]);
      MapName := ChangeFileExt(ImageName, '');
//      ImageName := WebDir + ImageName;

      Limit := High(Word) div 2;
      SavedShowFileName := cbShowFileName.IsChecked;
      SavedDepth := spnbxExpandLimit.Value;
      try
        if (pb1.Height > Limit) or (pb1.Width > Limit) then
        begin
          if cbShowFileName.IsChecked then
          begin
            cbShowFileName.IsChecked := False;
            cbShowFileNameChange(nil);
          end;
        end;
        while ((pb1.Height > Limit) or (pb1.Width > Limit))
          and (spnbxExpandLimit.Value > 0) do
        begin
          spnbxExpandLimit.Value := spnbxExpandLimit.Value -1;
          spnbxExpandLimitChange(nil);
        end;

        while spnbxExpandLimit.Value >= 0 do
        begin
          try
            SaveBitmap(WebDir + ImageName);
          except on EBitmapSizeTooBig do
            begin
              spnbxExpandLimit.Value := spnbxExpandLimit.Value -1;
              spnbxExpandLimitChange(nil);
              Continue;
            end;
          end;
          break;
        end;
        Lines.Add(Format(ImageLineS, [Item.Text, ImageName, MapName, Round(pb1.Height), Round(pb1.Width)]));
  //      Root := Item.FDisplayItem as TDisplayItem;
        WriteImageMap(Lines);
        Lines.Add('<br><a href="#Top">Back to top</a>');
        Lines.Add('<br><a href="index.html">Back to main index</a>');
      finally
        cbShowFileName.IsChecked := SavedShowFileName;
        spnbxExpandLimit.Value := SavedDepth;

      end;
    end;

    for ChildIndex := 0 to Item.Count - 1 do
    begin
      AChild := Item.Items[ChildIndex] as TFortranTreeViewItem;
      HandleChildren(AChild, Indent + 2, HLevel + 1)
    end;
  end;

begin
  if Selected <> nil then
  begin
    WebDir := IncludeTrailingPathDelimiter(ExtractFileDir(WebFileName));
    Lines.Assign(strstWeb1.Strings);
    Title := Selected.FortranItem.IndexName;
    Lines.Add(Format(TitleS, [Title]));

    Lines.AddStrings(strstWeb2.Strings);
    Lines.Add(Format(HeadS, [Title]));
    Lines.Add('<br><a href="index.html">Back to main index</a>');

    HandleChildrenTOC(Selected, 0, '', Lines);

    Lines.Add('    <p><br>');
    Lines.Add('    </p>');

    HandleChildren(Selected, 0, 2);

    Lines.AddStrings(strstWeb3.Strings);
  end;
end;

procedure TfrmMainFM.btnSaveWebSiteClick(Sender: TObject);
const
  TitleS = '    <title>%s</title>';
  H1S = '    <h1>%s</h1>';
  LinkS = '      <li><a href="%0:s">%1:s</a><br></li>';
var
  Lines: TStringList;
  ItemIndex: Integer;
  Item: TFortranTreeViewItem;
  Title : string;
  Dir: string;
  FileTitle: string;
  FileName: string;
begin
  Dir := GetCurrentDir;
  if SelectDirectory('Select Directory for Web Site', '', Dir) then
  begin
    Dir := IncludeTrailingPathDelimiter(Dir);
    Title := edTitle.Text;
    Lines := TStringList .Create;
    try
      Lines.Assign(strstWebIndex1.Strings);
      Lines.Add(Format(TitleS, [Title]));

      Lines.AddStrings(strstWebIndex2.Strings);
      Lines.Add(Format(H1S, [Title]));
      Lines.Add('    <ul>');

      for ItemIndex := 0 to tv1.Count - 1 do
      begin
        Item:= tv1.Items[ItemIndex] as TFortranTreeViewItem;
        FileTitle := ChangeFileExt(Item.Text, '');
        FileName := ChangeFileExt(Item.Text, '.html');
        Lines.Add(Format(LinkS, [FileName, FileTitle]));
        FileName := Dir + FileName;
        SaveWebPage(Item, FileName);
      end;

      Lines.AddStrings(strstWebIndex3.Strings);
      Lines.SaveToFile(Dir + 'index.html');
    finally
      Lines.Free;
    end;
  end;
end;

procedure TfrmMainFM.btnSaveImageClick(Sender: TObject);
var
  AFileName: TFileName;
begin
  if sd1.Execute then
  begin
    AFileName := sd1.FileName;
    SaveBitmap(AFileName);
  end;
end;

procedure TfrmMainFM.cbShowFileNameChange(Sender: TObject);
var
  ItemClass: TFortranProgramUnit;
begin
  for ItemClass in FDictionary.Values do
  begin
    ItemClass.ShowFileName := cbShowFileName.IsChecked;
  end;
  UpdatePaintBoxSize;
end;

procedure TfrmMainFM.FormCreate(Sender: TObject);
begin
  // FDisplayItemList := TDisplayItemList.Create(nil, pContained);
  FRoot := nil;
  miFile.Font.SIZE := 14;
  miFile.Text := 'File';
  miOpen.Font.SIZE := 14;
end;

procedure TfrmMainFM.ClearTreeView;
  procedure DeleteChildren(AParent: TFortranTreeViewItem);
  var
    ChildIndex: Integer;
    Child: TFortranTreeViewItem;
  begin
    for ChildIndex := 0 to AParent.Count - 1 do
    begin
      Child := AParent.ItemByIndex(ChildIndex) as TFortranTreeViewItem;
      if Assigned(Child.DisplayItem) then
      begin
        (Child.DisplayItem as TDisplayItem).DeleteLinks;
        Child.DisplayItem := nil;
      end;
      DeleteChildren(Child);
    end;
  end;
var
  ItemIndex: Integer;
  AnItem: TFortranTreeViewItem;
begin
  for ItemIndex := 0 to tv1.Count - 1 do
  begin
    AnItem := tv1.ItemByIndex(ItemIndex) as TFortranTreeViewItem;
    if Assigned(AnItem.DisplayItem) then
    begin
      (AnItem.DisplayItem as TDisplayItem).DeleteLinks;
      AnItem.DisplayItem := nil;
    end;
    DeleteChildren(AnItem);
    // AnItem.
  end;
  tv1.Clear;
end;

procedure TfrmMainFM.FormDestroy(Sender: TObject);
begin
  ClearTreeView;
  // FDisplayItemList := nil;
  FDictionary.Free;
end;

procedure TfrmMainFM.miOpenClick(Sender: TObject);
var
  Extractor: TExtractor;
  FileIndex: Integer;
  AUnit: TFortranProgramUnit;
  ContainedItemIndex: Integer;
  ContainedUnit: TFortranProgramUnit;
  FileItem: TFortranTreeViewItem;
  ContainedItem: TFortranTreeViewItem;
  Modules: TFortranTreeViewItem;
  Programs: TFortranTreeViewItem;
  Subroutines: TFortranTreeViewItem;
  Functions: TFortranTreeViewItem;
  BlockDatas: TFortranTreeViewItem;
  AFile: string;
  ParentItem: TFortranTreeViewItem;
begin
  if dlgOpen.Execute then
  begin
    Root := nil;
    ClearTreeView;
    Extractor := TExtractor.Create;
    try
      Extractor.FileNames.AddStrings(dlgOpen.Files);
      FDictionary.Free;
      FDictionary := TFortranUnitDictionary.Create([doOwnsValues]);
      try
        Extractor.FindUnits(FDictionary);
      except on E: EFortranReader do
        begin
          Beep;
          MessageDlg(E.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
          Exit;
        end;
      end;
      tv1.BeginUpdate;
      try
        for FileIndex := 0 to dlgOpen.Files.Count - 1 do
        begin
          AFile := dlgOpen.Files[FileIndex];
          if FDictionary.TryGetValue(AFile, AUnit) then
          begin
            FileItem := TFortranTreeViewItem.Create(self);
            FileItem.Font.SIZE := 14;
            FileItem.Text := AUnit.IndexName;
            FileItem.FortranItem := AUnit;
            tv1.AddObject(FileItem);

            Modules := nil;
            Programs := nil;
            Subroutines := nil;
            Functions := nil;
            BlockDatas := nil;
            // FileItem := nil;
            ParentItem := nil;

            for ContainedItemIndex := 0 to AUnit.Contains.Count - 1 do
            begin
              ContainedUnit := AUnit.Contains[ContainedItemIndex];
              case ContainedUnit.Kind of
                fbkProgram:
                  begin
                    if Programs = nil then
                    begin
                      Programs := TFortranTreeViewItem.Create(self);
                      Programs.Font.SIZE := 14;
                      Programs.Text := 'Programs';
                      FileItem.AddObject(Programs);
                    end;
                    ParentItem := Programs;
                  end;
                fbkSubroutine:
                  begin
                    if Subroutines = nil then
                    begin
                      Subroutines := TFortranTreeViewItem.Create(self);
                      Subroutines.Font.SIZE := 14;
                      Subroutines.Text := 'Subroutines';
                      FileItem.AddObject(Subroutines);
                    end;
                    ParentItem := Subroutines;
                  end;
                fbkFunction:
                  begin
                    if Functions = nil then
                    begin
                      Functions := TFortranTreeViewItem.Create(self);
                      Functions.Font.SIZE := 14;
                      Functions.Text := 'Functions';
                      FileItem.AddObject(Functions);
                    end;
                    ParentItem := Functions;
                  end;
                fbkModule:
                  begin
                    if Modules = nil then
                    begin
                      Modules := TFortranTreeViewItem.Create(self);
                      Modules.Font.SIZE := 14;
                      Modules.Text := 'Modules';
                      FileItem.AddObject(Modules);
                    end;
                    ParentItem := Modules;
                  end;
                fbkBlockData:
                  begin
                    if BlockDatas = nil then
                    begin
                      BlockDatas := TFortranTreeViewItem.Create(self);
                      BlockDatas.Font.SIZE := 14;
                      BlockDatas.Text := 'BlockData';
                      FileItem.AddObject(BlockDatas);
                    end;
                    ParentItem := BlockDatas;
                  end;
                fbkFile:
                  Assert(False);
              end;
              ContainedItem := TFortranTreeViewItem.Create(self);
              ContainedItem.Font.SIZE := 14;
              ContainedItem.Text := ContainedUnit.IndexName;
              ContainedItem.FortranItem := ContainedUnit;
              ParentItem.AddObject(ContainedItem);
            end;
          end
          else
          begin
            Beep;
            MessageDlg(AFile + ' is empty', TMsgDlgType.mtError,
              [TMsgDlgBtn.mbOK], 0);
          end;
        end;
      finally
        tv1.EndUpdate;
      end;
    finally
      Extractor.Free;
    end;
  end;
end;

procedure TfrmMainFM.pb1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Handled: TDisplaySelect;
  Item: TDisplayItem;
begin
  if Assigned(FRoot) then
  begin
    FRoot.HandleClick(Sender, X, Y, Handled, Item);
    if Handled <> dsNone then
    begin
      if Handled = dsSelect then
      begin
        Root := Item;
      end;
      UpdatePaintBoxSize;
    end;
  end;
end;

procedure TfrmMainFM.pb1Paint(Sender: TObject; Canvas: TCanvas);
var
  // ALine: Integer;
  // AParent: TDisplayItem;
  CallGraph: TCallGrapher;
  ASize: TSize;
  ItemSize: TSize;
  APoint: TShapePosition;
begin
  Canvas.BeginScene;
  try
  Canvas.Font.Size := 18;
  Canvas.Font.Family := 'Times New Roman';
  Canvas.Font.Style := [];

  Canvas.Fill.Kind := TBrushKind.bkSolid;
  Canvas.Fill.Color := claWhite;
  Canvas.FillRect(RectF(0, 0, pb1.Width, pb1.Height), 0, 0, [], 1);
  // RectF(0,0,Width,Height)

  if Assigned(Root) then
  begin
    // AParent := FDisplayItemList[0] as TDisplayItem;
    // AParent.HideChildren;
    // AParent := FDisplayItemList[ALine] as TDisplayItem;
    // FRoot := AParent;
    scrlbx1.BeginUpdate;
    CallGraph := TCallGrapher.Create;
    try
      CallGraph.ResursiveList := TDisplayItemList.Create(nil, pContained);
      CallGraph.FocusedItem := Root;
      ASize := CallGraph.AncestorSize(Root);
      ItemSize := Root.Size;
      APoint.X := 20 + ASize.cx - ItemSize.cx div 2;
      APoint.Y := 20;
      Root.Position := APoint;
      CallGraph.Paint(Sender, Canvas);
    finally
      CallGraph.Free;
      scrlbx1.EndUpdate;
    end;
  end
  finally
    Canvas.EndScene;

  end;
end;

procedure TfrmMainFM.SetRoot(const Value: TDisplayItem);
begin
  FRoot := Value;
  UpdatePaintBoxSize;
end;

procedure TfrmMainFM.spnbxExpandLimitChange(Sender: TObject);
var
  StartingLevel: Integer;
  MaxValue: Extended;
  IList: IDisplayItemList;
  procedure HandleDescendants(Item: TDisplayItem; Level: Integer);
  var
    ItemIndex: Integer;
    Child: TDisplayItem;
  begin
    if IList.IsRecursiveItem(Item) then
    begin
      Item.DescendantsExpanded := False;
      Exit;
    end;
    IList.Add(Item);
    try
      Item.DescendantsExpanded := Level < MaxValue;
      for ItemIndex := 0 to Item.Descendants.Count - 1 do
      begin
        Child := Item.Descendants[ItemIndex] as TDisplayItem;
        HandleDescendants(Child, Level + 1);
      end;
    finally
      IList.DeleteLast
    end;
  end;
  procedure HandleAncestors(Item: TDisplayItem; Level: Integer);
  var
    ItemIndex: Integer;
    Child: TDisplayItem;
  begin
    if IList.IsRecursiveItem(Item) then
    begin
      Item.DescendantsExpanded := False;
      Exit;
    end;
      IList.Add(Item);
      try
      Item.AncestorsExpanded := Level < MaxValue;
      for ItemIndex := 0 to Item.Ancestors.Count - 1 do
      begin
        Child := Item.Ancestors[ItemIndex] as TDisplayItem;
        HandleAncestors(Child, Level + 1);
      end;
    finally
      IList.DeleteLast
    end;
  end;

begin
  if Assigned(Root) then
  begin
    IList := TDisplayItemList.Create(nil,pContained );
    StartingLevel := 0;
    MaxValue := spnbxExpandLimit.Value;
    HandleDescendants(Root, StartingLevel);
    HandleAncestors(Root, StartingLevel);
    UpdatePaintBoxSize;
  end;
end;

procedure TfrmMainFM.tv1Change(Sender: TObject);
var
  AFortranUnit: TFortranProgramUnit;
  SelectedItem: TFortranTreeViewItem;
//  DisplayItem: TDisplayItem;
  ChildI: IDisplayItem;
begin
  if tv1.Selected <> nil then
  begin
    SelectedItem := tv1.Selected as TFortranTreeViewItem;
    AFortranUnit := SelectedItem.FortranItem;
    if Assigned(AFortranUnit) then
    begin
      if AFortranUnit.Kind <> fbkFile then
      begin
        if SelectedItem.DisplayItem = nil then
        begin
          SelectedItem.DisplayItem := TDisplayItem.Create(pb1, AFortranUnit);
          ChildI := SelectedItem.DisplayItem;
          // FDisplayItemList.Add(SelectedItem.DisplayItem);
        end;
        Root := SelectedItem.DisplayItem as TDisplayItem;
      end;
    end;
  end;

end;

procedure TfrmMainFM.UpdatePaintBoxSize;
var
  ASize: TSize;
  CallGraph: TCallGrapher;
begin
  if Assigned(Root) then
  begin
    pb1.Canvas.Font.Size := 18;
    pb1.Canvas.Font.Family := 'Times New Roman';
    pb1.Canvas.Font.Style := [];
    pb1.Canvas.Fill.Kind := TBrushKind.bkSolid;
    pb1.Canvas.Fill.Color := claWhite;
    CallGraph := TCallGrapher.Create;
    try
      CallGraph.ResursiveList := TDisplayItemList.Create(nil, pContained);
      CallGraph.FocusedItem := FRoot;
      ASize := CallGraph.GraphSize;
      pb1.Width := ASize.Width + 40;
      pb1.Height := ASize.Height + 40;
      pb1.InvalidateRect(RectF(0, 0, pb1.Width, pb1.Height));
//      scrlbx1.HScrollBar.Max := ASize.Width + 40;
//      scrlbx1.VScrollBar.Max := ASize.Height + 40;
    finally
      CallGraph.Free;
    end;
  end;
  pb1.InvalidateRect(RectF(0, 0, pb1.Width, pb1.Height));
end;

procedure TfrmMainFM.SaveBitmap(AFileName: TFileName);
var
  Png: TBitmap;
begin
  Png := TBitmap.Create(Round(pb1.Width), Round(pb1.Height));
  try
    pb1Paint(Png, Png.Canvas);
    Png.SaveToFile(AFileName);
  finally
    Png.Free;
  end;
end;

procedure TfrmMainFM.SaveWebPage(Selected: TFortranTreeViewItem; FileName: TFileName);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    WriteWebPage(Lines, FileName, Selected);
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

end.
