program FortranGrapherFM;

uses
  FastMM4 in 'FastMM4.pas',
  FMX.Forms,
  frmMainFMUnit in 'frmMainFMUnit.pas' {frmMainFM},
  ItemInterfaceUnit in 'ItemInterfaceUnit.pas',
  FortranCallGraphUnit in 'FortranCallGraphUnit.pas',
  FortranGrapherTypes in 'FortranGrapherTypes.pas',
  DisplayItemUnit in 'DisplayItemUnit.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  FortranReaderUnit in 'FortranReaderUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainFM, frmMainFM);
  Application.Run;
end.
