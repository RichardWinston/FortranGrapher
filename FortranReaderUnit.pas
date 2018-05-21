unit FortranReaderUnit;

interface

uses
  System.Generics.Collections, System.Classes, System.SysUtils, System.Math;

type
  TFortranBlockKind = (fbkProgram, fbkSubroutine, fbkFunction, fbkModule,
    fbkBlockData, fbkFile);

  EFortranReader = class(Exception);

  TFortranProgramUnit = class;

  TFortranUnitList = TList<TFortranProgramUnit>;
  TFortranUnitDictionary = TObjectDictionary<string, TFortranProgramUnit>;

  TToken = class(TObject)
    Name: string;
    LineIndex: Integer;
  end;

  TTokenList = TObjectList<TToken>;

  TFortranProgramUnit = class(TObject)
  private
    FKind: TFortranBlockKind;
    FIsCalledBy: TFortranUnitList;
    FCalls: TFortranUnitList;
    FContains: TFortranUnitList;
    FContainedBy: TFortranUnitList;
    FName: string;
    FStartLine: Integer;
    FFileName: string;
    FShowFileName: Boolean;
    FCommentLines: TStrings;
    function GetDisplayName: string;
    function GetIndexName: string;
    procedure SetCommentLines(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    property Kind: TFortranBlockKind read FKind write FKind;
    property IsCalledBy: TFortranUnitList read FIsCalledBy;
    property Calls: TFortranUnitList read FCalls;
    property Contains: TFortranUnitList read FContains;
    property ContainedBy: TFortranUnitList read FContainedBy;
    property Name: string read FName write FName;
    property DisplayName: string read GetDisplayName;
    property StartLine: Integer read FStartLine write FStartLine;
    property FileName: string read FFileName write FFileName;
    property ShowFileName: Boolean read FShowFileName write FShowFileName;
    property IndexName: string read GetIndexName;
    property CommentLines: TStrings read FCommentLines write SetCommentLines;
  end;

  TExtractor = class(TObject)
  private
    FFileNames: TStrings;
    procedure FindProgramUnits(const FileName: string;
      Dictionary: TFortranUnitDictionary); overload;
    function ExtractCode(ALine: string): string;
    procedure ExtractTokens(ACodeLine: string; Tokens: TTokenList;
      LineIndex: Integer; var ALabel: string; var Continued, InString: Boolean);
    procedure FindLinks(const FileName: string;
      Dictionary: TFortranUnitDictionary);

  const
    WhiteSpace = [' ' { , #9 } ];
    InitialCommentMarkers = ['C', '!', 'c', '*'];
    CharacterDelimiters = ['''', '"'];
    TokenDelimiters = [' ', '(', ')', ',', '=', '.', '+', '-', '/', '*', ':'];
  public
    constructor Create;
    destructor Destroy; override;
    property FileNames: TStrings read FFileNames;
    // @name calls @link(FindProgramUnits) and @link(FindLinks) for every
    // file in @link(FileNames).
    procedure FindUnits(Dictionary: TFortranUnitDictionary);
  end;

implementation

var
  TestTokens: TStringList;

  { TFortranProgramUnit }

constructor TFortranProgramUnit.Create;
begin
  inherited;
  FShowFileName := True;
  FCalls := TFortranUnitList.Create;
  FIsCalledBy := TFortranUnitList.Create;
  FContains := TFortranUnitList.Create;
  FContainedBy := TFortranUnitList.Create;
  FCommentLines := TStringList.Create;
end;

destructor TFortranProgramUnit.Destroy;
begin
  FCommentLines.Free;
  FContainedBy.Free;
  FContains.Free;
  FIsCalledBy.Free;
  FCalls.Free;
  inherited;
end;

function TFortranProgramUnit.GetDisplayName: string;
begin
  case Kind of
    fbkProgram, fbkSubroutine, fbkFunction, fbkModule, fbkBlockData:
      begin
        if ShowFileName then
        begin
          result := Name + sLineBreak + FileName;
        end
        else
        begin
          result := Name;
        end;
      end;
    fbkFile:
      begin
        result := FileName;
      end;
  else
    Assert(False);
  end;
end;

function TFortranProgramUnit.GetIndexName: string;
begin
  case Kind of
    fbkProgram, fbkSubroutine, fbkFunction, fbkModule, fbkBlockData:
      begin
        result := Name;
      end;
    fbkFile:
      begin
        result := FileName;
      end;
  else
    Assert(False);
  end;
end;

procedure TFortranProgramUnit.SetCommentLines(const Value: TStrings);
begin
  FCommentLines.Assign(Value);
end;

function TExtractor.ExtractCode(ALine: string): string;
var
  InString: Boolean;
  CharIndex: Integer;
  LastChar: Integer;
  StringDelimiter: Char;
  AChar: Char;
  Prefix: string;
  Code: string;
  index: Integer;
  SB: TStringBuilder;
begin
  result := '';
  if Length(ALine) > 0 then
  begin
    Prefix := Copy(ALine,1,6);
    Code := Copy(ALine,7,MaxInt);
    SB := TStringBuilder.Create;
    try
      for index := 1 to Length(Prefix) do
      begin
        AChar := Prefix[index];
        if AChar = #9 then
        begin
          SB.Append(StringOfChar(' ', (7-index)));
        end
        else
        begin
          SB.Append(AChar);
        end;
      end;
      Prefix := SB.ToString;
    finally
      SB.Free;
    end;
    ALine := Prefix + Code;
    if CharInSet(ALine[1], InitialCommentMarkers) then
    begin
      Exit;
    end;
    InString := False;
    LastChar := 72;
    StringDelimiter := '''';
    for CharIndex := 1 to Min(72, Length(ALine)) do
    begin
      AChar := ALine[CharIndex];
      if (AChar = '!') and not InString then
      begin
        LastChar := CharIndex - 1;
        Break;
      end;
      if CharIndex > 6 then
      begin
        if CharInSet(AChar, CharacterDelimiters) then
        begin
          if InString and (AChar = StringDelimiter) then
          begin
            InString := False;
          end
          else
          begin
            InString := True;
            StringDelimiter := AChar;
          end;
        end;
      end;
    end;
    result := Copy(ALine, 1, LastChar);
  end;
end;

procedure TExtractor.ExtractTokens(ACodeLine: string; Tokens: TTokenList;
  LineIndex: Integer; var ALabel: string; var Continued, InString: Boolean);
var
  CharIndex: Integer;
  AChar: Char;
  LocalInString: Boolean;
  StringDelimiter: Char;
  TokenStart: Integer;
  SkippedToken: Boolean;
  ATokenName: string;
  procedure AddToken;
  var
    AToken: TToken;
    TokenIndex: Integer;
  begin
    Assert(ATokenName <> '');
    if TestTokens.IndexOf(ATokenName) >= 0 then
    begin
      if (Tokens.Count > 0) and (UpperCase(Tokens[Tokens.Count - 1].Name)
        = 'END') then
      begin
        SkippedToken := True;
        if ATokenName = 'MODULE' then
        begin
          AToken := TToken.Create;
          AToken.Name := '';
          AToken.LineIndex := LineIndex;
          Tokens.Add(AToken)
        end;
      end
      else
      begin
        if ATokenName = 'FUNCTION' then
        begin
          for TokenIndex := 0 to Tokens.Count - 1 do
          begin
            Tokens[TokenIndex].Free;
          end;
          Tokens.Clear;
        end;
        SkippedToken := False;
        AToken := TToken.Create;
        AToken.Name := ATokenName;
        AToken.LineIndex := LineIndex;
        Tokens.Add(AToken)
      end;
    end
    else
    begin
      if SkippedToken then
      begin
        SkippedToken := False;
      end
      else
      begin
        AToken := TToken.Create;
        AToken.Name := ATokenName;
        AToken.LineIndex := LineIndex;
        Tokens.Add(AToken)
      end;
    end;
  end;

begin
  ALabel := '';
  Continued := False;
  Tokens.Clear;
  StringDelimiter := '''';
  LocalInString := False;
  TokenStart := 1;
  for CharIndex := 1 to Length(ACodeLine) do
  begin
    AChar := ACodeLine[CharIndex];
    if CharIndex > 6 then
    begin
      if CharInSet(AChar, CharacterDelimiters) then
      begin
        if LocalInString and (AChar = StringDelimiter) then
        begin
          LocalInString := False;
        end
        else
        begin
          if not LocalInString then
          begin
            LocalInString := True;
            StringDelimiter := AChar;
          end;
        end;
      end;
    end;
    if not LocalInString then
    begin
      if CharInSet(AChar, TokenDelimiters) then
      begin
        if (TokenStart < CharIndex) then
        begin
          if TokenStart > 6 then
          begin
            ATokenName :=
              Trim(Copy(ACodeLine, TokenStart, CharIndex - TokenStart));
            if ATokenName <> '' then
            begin
              AddToken;
            end;
          end
          else if TokenStart <> 6 then
          begin
            ALabel := Trim(Copy(ACodeLine, TokenStart, CharIndex - TokenStart));
          end;
        end;
        TokenStart := CharIndex + 1;
      end;
      if (CharIndex = 6) and not CharInSet(AChar, WhiteSpace) then
      begin
        Continued := Trim(Copy(ACodeLine, 1, 5)) = '';
        TokenStart := 7;
        if Continued then
        begin
          LocalInString := InString;
        end;
      end;
      if CharIndex = Length(ACodeLine) then
      begin
        if (TokenStart < CharIndex + 1) then
        begin
          if TokenStart > 6 then
          begin
            ATokenName :=
              Trim(Copy(ACodeLine, TokenStart, CharIndex + 1 - TokenStart));
            if ATokenName <> '' then
            begin
              AddToken;
            end;
          end
          else if TokenStart <> 6 then
          begin
            ALabel := Copy(ACodeLine, TokenStart, CharIndex + 1 - TokenStart);
          end;
        end;
        TokenStart := CharIndex + 1;
      end;
    end;
  end;
  InString := LocalInString;
end;

{ TExtractor }

constructor TExtractor.Create;
begin
  FFileNames := TStringList.Create;
end;

destructor TExtractor.Destroy;
begin
  FFileNames.Free;
  inherited;
end;

procedure TExtractor.FindLinks(const FileName: string;
  Dictionary: TFortranUnitDictionary);
var
  AFile: TStringList;
  LineIndex: Integer;
  ALine: string;
  Tokens: TTokenList;
  ALabel: string;
  Continued: Boolean;
  InString: Boolean;
  TokenIndex: Integer;
  AToken: string;
  AUnit: TFortranProgramUnit;
  UnitName: string;
  FoundProgramUnit: Boolean;
  TokenType: Integer;
  CalledUnit: TFortranProgramUnit;
  LocalTokens: TTokenList;
//  T: TToken;
  FoundPrevious: Boolean;
  InInterface: Boolean;
begin
  AUnit := nil;
  AFile := TStringList.Create;
  Tokens := TTokenList.Create;
  LocalTokens := TTokenList.Create(False);
  try
    AFile.LoadFromFile(FileName);
    if AFile.Count = 0 then
    begin
      Exit;
    end;
    InString := False;
    for LineIndex := 0 to AFile.Count - 1 do
    begin
      ALine := AFile[LineIndex];
      ALine := ExtractCode(ALine);
      if ALine <> '' then
      begin
        ExtractTokens(ALine, LocalTokens, LineIndex, ALabel, Continued,
          InString);
        Tokens.AddRange(LocalTokens);
      end;
    end;
    if Tokens.Count > 0 then
    begin
      FoundPrevious := False;
      InInterface := False;
      for TokenIndex := 0 to Tokens.Count - 1 do
      begin
//        T := Tokens[TokenIndex];
//        Assert(T <> nil);
//        Assert(T.Name <> '');
        AToken := UpperCase(Tokens[TokenIndex].Name);
        if InInterface then
        begin
          if AToken = 'ENDINTERFACE' then
          begin
            InInterface := False;
          end
          else if (AToken = 'INTERFACE') and (UpperCase(Tokens[TokenIndex-1].Name) = 'END') then
          begin
            InInterface := False;
          end;
          Continue;
        end;
        if AToken = 'INTERFACE' then
        begin
          InInterface := True;
          Continue;
        end;
        FoundProgramUnit := False;
        TokenType := TestTokens.IndexOf(AToken);
        if TokenType >= 0 then
        begin
          if (TokenIndex <= 0) OR (UpperCase(Tokens[TokenIndex - 1].Name) <>
            'INTERFACE') then
          begin
            Assert(Tokens.Count - 1 > TokenIndex);
            UnitName := Tokens[TokenIndex + 1].Name;
            FoundProgramUnit := True;
            FoundPrevious := True;
          end;
        end
        else if TokenIndex = 0 then
        begin
          UnitName := 'Main Program';
          FoundProgramUnit := True;
          FoundPrevious := True;
        end
        else if (TokenIndex > 0) and (Tokens[TokenIndex - 1].Name = '') then
        begin
          UnitName := 'Main Program';
          FoundProgramUnit := True;
          FoundPrevious := True;
        end;
        if FoundProgramUnit then
        begin
          AUnit := Dictionary[UpperCase(UnitName)];
          // FoundPrevious := False;
        end
        else if Dictionary.TryGetValue(UpperCase(AToken), CalledUnit) then
        begin
          if not FoundPrevious then
          begin
            if (AUnit.Kind in [fbkProgram, fbkSubroutine, fbkFunction]) and
              (CalledUnit.Kind in [fbkProgram, fbkSubroutine, fbkFunction]) then
            begin
              if AUnit.Calls.IndexOf(CalledUnit) < 0 then
              begin
                if (AUnit <> CalledUnit) then
                begin
                  AUnit.Calls.Add(CalledUnit);
                  CalledUnit.IsCalledBy.Add(AUnit);
                end;
              end;
            end;
          end;
          FoundPrevious := False;
        end
        else
        begin
          FoundPrevious := False;
        end;

      end;
    end;
  finally
    LocalTokens.Free;
    AFile.Free;
    Tokens.Free;
  end;
end;

procedure TExtractor.FindProgramUnits(const FileName: string;
  Dictionary: TFortranUnitDictionary);
var
  AFile: TStringList;
  LineIndex: Integer;
  ALine: string;
  Tokens: TTokenList;
  ALabel: string;
  Continued: Boolean;
  InString: Boolean;
  TokenIndex: Integer;
  FileUnit: TFortranProgramUnit;
  Kind: TFortranBlockKind;
  AToken: string;
  AUnit: TFortranProgramUnit;
  UnitName: string;
  CreateFortranUnit: Boolean;
  TokenType: Integer;
  LocalTokens: TTokenList;
  InInterface: Boolean;
  CommentFound: Boolean;
begin
  Kind := fbkProgram;
  AFile := TStringList.Create;
  LocalTokens := TTokenList.Create(False);
  Tokens := TTokenList.Create;
  try
    AFile.LoadFromFile(FileName);
    if AFile.Count > 0 then
    begin
      FileUnit := TFortranProgramUnit.Create;
      FileUnit.Name := FileName;
      FileUnit.FileName := ExtractFileName(FileName);
      FileUnit.Kind := fbkFile;
      Dictionary.Add(FileName, FileUnit);
    end
    else
    begin
      Exit;
    end;
    InString := False;
    for LineIndex := 0 to AFile.Count - 1 do
    begin
      ALine := AFile[LineIndex];
      ALine := ExtractCode(ALine);
      if ALine <> '' then
      begin
        ExtractTokens(ALine, LocalTokens, LineIndex, ALabel, Continued,
          InString);
        Tokens.AddRange(LocalTokens);
      end;
    end;
    if Tokens.Count > 0 then
    begin
      InInterface := False;
      for TokenIndex := 0 to Tokens.Count - 1 do
      begin
        AToken := UpperCase(Tokens[TokenIndex].Name);
        if InInterface then
        begin
          if AToken = 'ENDINTERFACE' then
          begin
            InInterface := False;
          end
          else if (AToken = 'INTERFACE') and (UpperCase(Tokens[TokenIndex-1].Name) = 'END') then
          begin
            InInterface := False;
          end;
          Continue;
        end;
        if AToken = 'INTERFACE' then
        begin
          InInterface := True;
          Continue;
        end;
        CreateFortranUnit := False;
        TokenType := TestTokens.IndexOf(AToken);
        if TokenType >= 0 then
        begin
          Kind := TFortranBlockKind(TokenType);
          if (TokenIndex <= 0) OR (UpperCase(Tokens[TokenIndex - 1].Name) <>
            'INTERFACE') then
          begin
            Assert(Tokens.Count - 1 > TokenIndex);
            UnitName := Tokens[TokenIndex + 1].Name;
            CreateFortranUnit := True;
          end;
        end
        else if TokenIndex = 0 then
        begin
          UnitName := 'Main Program';
          Kind := fbkProgram;
          CreateFortranUnit := True;
        end
        else if (TokenIndex > 0) and (Tokens[TokenIndex - 1].Name = '') then
        begin
          UnitName := 'Main Program';
          Kind := fbkProgram;
          CreateFortranUnit := True;
        end;

        if CreateFortranUnit then
        begin
          AUnit := TFortranProgramUnit.Create;
          AUnit.Name := UnitName;
          AUnit.FileName := FileUnit.DisplayName;
          AUnit.Kind := Kind;
          AUnit.StartLine := Tokens[TokenIndex].LineIndex;
          if Dictionary.ContainsKey(UpperCase(AUnit.Name)) then
          begin
            AUnit.Free;
            AUnit := Dictionary[UpperCase(UnitName)];
            raise EFortranReader.Create('Error adding ' + UnitName + ' on line '
              + IntToStr(Tokens[TokenIndex + 1].LineIndex + 1) + ' of ' +
              FileUnit.DisplayName + '. The previous occurance was' + ' on line'
              + IntToStr(AUnit.StartLine + 1) + ' of ' + AUnit.FileName + '.');

          end;
          Dictionary.Add(UpperCase(AUnit.Name), AUnit);
          FileUnit.FContains.Add(AUnit);
          AUnit.ContainedBy.Add(FileUnit);
          if AUnit.StartLine >= 0 then
          begin
            CommentFound := False;
            for LineIndex := AUnit.StartLine -1 downto 0 do
            begin
              ALine := AFile[LineIndex];
              if (Length(ALine) > 0) and ((ALine[1] = 'C')
                or (ALine[1] = 'c') or (ALine[1] = '!')) then
              begin
                CommentFound := True;
                AUnit.CommentLines.Insert(0, ALine);
              end
              else
              begin
                if CommentFound then
                begin
                  break;
                end;
              end;
            end;
            CommentFound := False;
            for LineIndex := AUnit.StartLine +1 to AFile.Count -1 do
            begin
              ALine := AFile[LineIndex];
              if (Length(ALine) > 0) and ((ALine[1] = 'C')
                or (ALine[1] = 'c') or (ALine[1] = '!')) then
              begin
                CommentFound := True;
                AUnit.CommentLines.Add(ALine);
              end
              else
              begin
                if CommentFound then
                begin
                  break;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    AFile.Free;
    LocalTokens.Free;
    Tokens.Free;
  end;
end;

procedure TExtractor.FindUnits(Dictionary: TFortranUnitDictionary);
var
  FileIndex: Integer;
begin
  for FileIndex := 0 to FileNames.Count - 1 do
  begin
    FindProgramUnits(FileNames[FileIndex], Dictionary);
  end;
  for FileIndex := 0 to FileNames.Count - 1 do
  begin
    FindLinks(FileNames[FileIndex], Dictionary);
  end;
end;

initialization

TestTokens := TStringList.Create;
TestTokens.Add('PROGRAM');
TestTokens.Add('SUBROUTINE');
TestTokens.Add('FUNCTION');
TestTokens.Add('MODULE');
TestTokens.Add('BLOCKDATA');

finalization

TestTokens.Free;

end.
