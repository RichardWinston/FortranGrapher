unit frmFortranEditorUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus, FMX.Layouts,
  FMX.Memo, FMX.TreeView, FortranReaderUnit;

type
  TfrmFortranEditor = class(TForm)
    procedure miOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDictionary: TFortranUnitDictionary;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFortranEditor: TfrmFortranEditor;

implementation

uses
  System.Generics.Collections;



{$R *.fmx}

procedure TfrmFortranEditor.FormDestroy(Sender: TObject);
begin
  FDictionary.Free;
end;

procedure TfrmFortranEditor.miOpenClick(Sender: TObject);
var
  Extractor: TExtractor;
  FileIndex: Integer;
  AUnit: TFortranProgramUnit;
  ContainedItemIndex: Integer;
  ContainedUnit: TFortranProgramUnit;
  FileItem: TTreeViewItem;
  ContainedItem: TTreeViewItem;
  Modules: TTreeViewItem;
  Programs: TTreeViewItem;
  Subroutines: TTreeViewItem;
  Functions: TTreeViewItem;
  BlockDatas: TTreeViewItem;
  AFile: string;
  ParentItem: TTreeViewItem;
begin
//  if dlgOpen.Execute then
//  begin
//    tv1.Clear;
//    Extractor := TExtractor.Create;
//    try
//      Extractor.FileNames.AddStrings(dlgOpen.Files);
//      FDictionary.Free;
//      FDictionary := TFortranUnitDictionary.Create([doOwnsValues]);
//      Extractor.FindUnits(FDictionary);
//      for FileIndex := 0 to dlgOpen.Files.Count - 1 do
//      begin
//        AFile := dlgOpen.Files[FileIndex];
//        if FDictionary.TryGetValue(AFile, AUnit) then
//        begin
//          FileItem := TTreeViewItem.Create(self);
//          FileItem.Text := AUnit.Name;
//          tv1.AddObject(FileItem);
//
//          Modules := nil;
//          Programs := nil;
//          Subroutines := nil;
//          Functions := nil;
//          BlockDatas := nil;
//
//          for ContainedItemIndex := 0 to AUnit.Contains.Count - 1 do
//          begin
//            ContainedUnit := AUnit.Contains[ContainedItemIndex];
//            case ContainedUnit.Kind of
//              fbkProgram:
//                begin
//                  if Programs = nil then
//                  begin
//                    Programs := TTreeViewItem.Create(self);
//                    Programs.Text := 'Programs';
//                    FileItem.AddObject(Programs);
//                  end;
//                  ParentItem := Programs;
//                end;
//              fbkSubroutine:
//                begin
//                  if Subroutines = nil then
//                  begin
//                    Subroutines := TTreeViewItem.Create(self);
//                    Subroutines.Text := 'Subroutines';
//                    FileItem.AddObject(Subroutines);
//                  end;
//                  ParentItem := Subroutines;
//                end;
//              fbkFunction:
//                begin
//                  if Functions = nil then
//                  begin
//                    Functions := TTreeViewItem.Create(self);
//                    Functions.Text := 'Functions';
//                    FileItem.AddObject(Functions);
//                  end;
//                  ParentItem := Functions;
//                end;
//              fbkModule:
//                begin
//                  if Modules = nil then
//                  begin
//                    Modules := TTreeViewItem.Create(self);
//                    Modules.Text := 'Modules';
//                    FileItem.AddObject(Modules);
//                  end;
//                  ParentItem := Modules;
//                end;
//              fbkBlockData:
//                begin
//                  if BlockDatas = nil then
//                  begin
//                    BlockDatas := TTreeViewItem.Create(self);
//                    BlockDatas.Text := 'BlockData';
//                    FileItem.AddObject(BlockDatas);
//                  end;
//                  ParentItem := BlockDatas;
//                end;
//              fbkFile: Assert(False);
//            end;
//            ContainedItem := TTreeViewItem.Create(self);
//            ContainedItem.Text := ContainedUnit.Name;
//            ParentItem.AddObject(ContainedItem);
//          end;
//        end
//        else
//        begin
//          Beep;
//          MessageDlg(AFile + ' is empty', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
//        end;
//      end;
//    finally
//      Extractor.Free;
//    end;
//  end;
end;

end.
