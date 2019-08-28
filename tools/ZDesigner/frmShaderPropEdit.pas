unit frmShaderPropEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, SynEdit, SynEditTypes, SynCompletionProposal,
  frmCustomPropEditBase;

type
  TShaderPropEditForm = class(TCustomPropEditBaseForm)
    ShaderPanel: TGroupBox;
    CompileShaderButton: TButton;
    CompileErrorLabel: TStaticText;
    Splitter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure EditorGutterPaint(Sender: TObject; aLine, X, Y: Integer);
  private
    FMousePos: TPoint;
    FOldCaretY: Integer;
    FUnderLine: Integer;
    FErrorLines: array of Integer;
    procedure OnShaderExprChanged(Sender: TObject);
    procedure EditorMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure EditorSpecialLineColors(Sender: TObject;
      Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure EditorStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure AutoCompOnExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
  public
    ShaderSynEdit : TSynEdit;
    AutoComp : TSynCompletionProposal;

    procedure SaveChanges; override;
    procedure ShowError(MsgText: String);
    procedure HideError;
  end;

implementation

{$R *.dfm}

uses
  System.Math, dmCommon, SynHighlighterGLSL, SynEditSearch;

procedure TShaderPropEditForm.FormCreate(Sender: TObject);
begin
  inherited;

  ShaderSynEdit := TSynEdit.Create(Self);
  ShaderSynEdit.Highlighter := TSynGLSLSyn.Create(Self);
  ShaderSynEdit.Align := alClient;
  ShaderSynEdit.Gutter.Visible := True;
  ShaderSynEdit.Gutter.ShowModification := True;
  ShaderSynEdit.Gutter.ShowLineNumbers := False;
  ShaderSynEdit.Parent := ShaderPanel;
  ShaderSynEdit.OnChange := OnShaderExprChanged;
  ShaderSynEdit.OnMouseMove := EditorMouseMove;
  ShaderSynEdit.OnStatusChange := EditorStatusChange;
  ShaderSynEdit.OnSpecialLineColors := EditorSpecialLineColors;
  ShaderSynEdit.OnGutterPaint := EditorGutterPaint;
  ShaderSynEdit.WantTabs := True;
  ShaderSynEdit.TabWidth := 2;
  ShaderSynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  ShaderSynEdit.SearchEngine := TSynEditSearch.Create(Self);
  ShaderSynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;

  // SynEdit autocompletion
  AutoComp := TSynCompletionProposal.Create(Self);
  AutoComp.Editor := ShaderSynEdit;
  AutoComp.EndOfTokenChr := '+-/*=()[]., @';
  AutoComp.TriggerChars := 'abcdefghijklmnopqrstuvxyz.@';
  AutoComp.ShortCut := 16416;
  AutoComp.Options := DefaultProposalOptions + [scoCaseSensitive,
    scoUseBuiltInTimer, scoUseInsertList, scoUsePrettyText];
  AutoComp.TimerInterval := 2000;
  AutoComp.OnExecute := AutoCompOnExecute;
end;

type
  TGLSLType = (gtVoid, gtBool, gtInt, gtFloat, gtVec2, gtVec3, gtVec4,
    gtGenType, gtIVec2, gtIVec3, gtIVec4, gtGenIType,
    gtBVec2, gtBVec3, gtBVec4, gtGenBType,
    gtMat2, gtMat3, gtMat4, gtMat2x2, gtMat2x3, gtMat2x4,
    gtMat3x2, gtMat3x3, gtMat3x4, gtMat4x2, gtMat4x3, gtMat4x4);

  TArgument = record
    &Type: TGLSLType;
    Name: string;
  end;

function GLSLTypeToString(Value: TGLSLType): string;
begin
  case Value of
    gtVoid:
      Result := 'void';
    gtBool:
      Result := 'bool';
    gtInt:
      Result := 'int';
    gtFloat:
      Result := 'float';
    gtVec2:
      Result := 'vec2';
    gtVec3:
      Result := 'vec3';
    gtVec4:
      Result := 'vec4';
    gtIVec2:
      Result := 'ivec2';
    gtIVec3:
      Result := 'ivec3';
    gtIVec4:
      Result := 'ivec4';
    gtBVec2:
      Result := 'bvec2';
    gtBVec3:
      Result := 'bvec3';
    gtBVec4:
      Result := 'bvec4';
    gtMat2:
      Result := 'mat2';
    gtMat3:
      Result := 'mat3';
    gtMat4:
      Result := 'mat4';
    gtMat2x2:
      Result := 'mat2x2';
    gtMat2x3:
      Result := 'mat2x3';
    gtMat2x4:
      Result := 'mat2x4';
    gtMat3x2:
      Result := 'mat3x2';
    gtMat3x3:
      Result := 'mat3x3';
    gtMat3x4:
      Result := 'mat3x4';
    gtMat4x2:
      Result := 'mat4x2';
    gtMat4x3:
      Result := 'mat4x3';
    gtMat4x4:
      Result := 'mat4x4';
  end;
end;

function Argument(&Type: TGLSLType; Name: String): TArgument;
begin
  Result.&Type := &Type;
  Result.Name := Name;
end;


procedure TShaderPropEditForm.AutoCompOnExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  Comp: TSynCompletionProposal;
  Line: string;
  I: Integer;

  procedure InAdd(const Items : array of string);
  var
    S : string;
  begin
    for S in Items do
    begin
      Comp.InsertList.Add(S);
      Comp.ItemList.Add(S);
    end;
  end;

  procedure AddBasicInternalFunction(const Name: string);
  begin
    Comp.InsertList.Add(Name);
    Comp.ItemList.Add('float \style{+B}' + Name + '\style{-B}(float x)');
    Comp.InsertList.Add(Name);
    Comp.ItemList.Add('vec2 \style{+B}' + Name + '\style{-B}(vec2 x)');
    Comp.InsertList.Add(Name);
    Comp.ItemList.Add('vec3 \style{+B}' + Name + '\style{-B}(vec3 x)');
    Comp.InsertList.Add(Name);
    Comp.ItemList.Add('vec4 \style{+B}' + Name + '\style{-B}(vec4 x)');
  end;

  procedure AddBasicInternalFunctions(const Name: array of string);
  var
    S : string;
  begin
    for S in Name do
      AddBasicInternalFunction(S);
  end;

  procedure AddInternalFunctionEx(const Name: string; ReturnType: TGLSLType;
    Arguments: array of TArgument);
  var
    IsGenType: Boolean;
    Argument: TArgument;
    GenTypeValue: TGLSLType;
    Text: string;
  begin
    IsGenType := ReturnType = gtGenType;
    for Argument in Arguments do
      IsGenType := IsGenType or (Argument.&Type = gtGenType);

    if IsGenType then
    begin
      for GenTypeValue := gtFloat to gtVec4 do
      begin
        Comp.InsertList.Add(Name);
        if ReturnType <> gtGenType then
          Text := GLSLTypeToString(ReturnType)
        else
          Text := GLSLTypeToString(GenTypeValue);

        for Argument in Arguments do
        begin
          if Argument.&Type <> gtGenType then
            Text := Text + GLSLTypeToString(Argument.&Type)
          else
            Text := Text + GLSLTypeToString(GenTypeValue);
          Text := Text + ' ' + Argument.Name + ', ';
        end;

        if Length(Arguments) > 0 then
          Delete(Text, Length(Arguments) - 2, 2);

        Text := Text + ')';

        Text := Text + ' \style{+B}' + Name + '\style{-B}(';
      end;

      Comp.InsertList.Add(Name);
      Comp.ItemList.Add('vec4 \style{+B}' + Name + '\style{-B}(vec4 x)');
    end
    else
    begin
      Text := GLSLTypeToString(ReturnType) + ' \style{+B}' + Name + '\style{-B}(';

      for Argument in Arguments do
        Text := Text + GLSLTypeToString(Argument.&Type) + ' ' + Argument.Name + ', ';
      if Length(Arguments) > 0 then
        Delete(Text, Length(Arguments) - 2, 2);
      Text := Text + ')';

      Comp.InsertList.Add(Name);
      Comp.ItemList.Add(Text);
    end;
  end;

begin
  Comp := Sender as TSynCompletionProposal;
  Comp.ItemList.Clear;
  Comp.InsertList.Clear;

  Line := ShaderSynEdit.LineText;
  I := Min(ShaderSynEdit.CaretX - 1, Length(Line));
  while (I > 0) and CharInSet(Line[I],['a'..'z','A'..'Z','_','0'..'9']) do
    Dec(I);

  if (I > 0) and (Line[I] = '.') then
  begin
    InAdd(['x', 'y', 'z', 'w', 'xy', 'xx', 'yy', 'yx', 'zz', 'ww', 'yz', 'zw',
      'xz', 'xw', 'yw', 'wz', 'xyz', 'xyzw']);
  end
  else
  begin
    InAdd(['active', 'asm', 'atomic_uint', 'attribute', 'bool', 'break',
      'buffer', 'bvec2', 'bvec3', 'bvec4', 'case', 'cast', 'centroid', 'class',
      'coherent', 'common', 'const', 'continue', 'def', 'default', 'discard',
      'dmat2', 'dmat2x2', 'dmat2x3', 'dmat2x4', 'dmat3', 'dmat3x2', 'dmat3x3',
      'dmat3x4', 'dmat4', 'dmat4x2', 'dmat4x3', 'dmat4x4', 'do', 'double',
      'dvec2', 'dvec3', 'dvec4', 'else', 'enum', 'extern', 'external', 'false',
      'filter', 'fixed', 'flat', 'float', 'for', 'fvec2', 'fvec3', 'fvec4',
      'goto', 'half', 'highp', 'hvec2', 'hvec3', 'hvec4', 'if', 'iimage1d',
      'iimage1darray', 'iimage2d', 'iimage2darray', 'iimage2dms',
      'iimage2dmsarray', 'iimage2drect', 'iimage3d', 'iimagebuffer',
      'iimagecube', 'iimagecubearray', 'image1d', 'image1darray', 'image2d',
      'image2darray', 'image2dms', 'image2dmsarray', 'image2drect', 'image3d',
      'imagebuffer', 'imagecubearray', 'in', 'inline', 'inout', 'input', 'int',
      'interface', 'invariant', 'isampler1d', 'isampler1darray', 'isampler2d',
      'isampler2darray', 'isampler2dms', 'isampler2dmsarray', 'isampler2drect',
      'isampler3d', 'isamplerbuffer', 'isamplercube', 'isamplercubearray',
      'isubpassinput', 'isubpassinputms', 'itexture1d', 'itexture1darray',
      'itexture2d', 'itexture2darray', 'itexture2dms', 'itexture2dmsarray',
      'itexture2drect', 'itexture3d', 'itexturebuffer', 'itexturecube',
      'itexturecubearray', 'ivec2', 'ivec3', 'ivec4', 'layout', 'long', 'lowp',
      'mat2', 'mat2x2', 'mat2x3', 'mat2x4', 'mat3', 'mat3x2', 'mat3x3',
      'mat3x4', 'mat4', 'mat4x2', 'mat4x3', 'mat4x4', 'mediump', 'namespace',
      'noinline', 'noperspective', 'out', 'output', 'partition', 'patch',
      'precise', 'precision', 'public', 'readonly', 'resource', 'restrict',
      'return', 'sample', 'sampler', 'sampler1d', 'sampler1darray',
      'sampler1darrayshadow', 'sampler1dshadow', 'sampler2d', 'sampler2darray',
      'sampler2darrayshadow', 'sampler2dms', 'sampler2dmsarray',
      'sampler2drectshadow', 'sampler2dshadow', 'sampler3d', 'sampler3drect',
      'samplerbuffer', 'samplercubearray', 'samplercubearrayshadow',
      'samplercubeshadow', 'samplershadow', 'shared', 'short', 'sizeof',
      'smooth', 'static', 'struct', 'subpassinput', 'subpassinputms',
      'subroutine', 'superp', 'switch', 'template', 'texture1d',
      'texture1darray', 'texture2d', 'texture2darray', 'texture2dms',
      'texture2dmsarray', 'texture2drect', 'texture3d', 'texturebuffer',
      'texturecube', 'texturecubearray', 'this', 'true', 'type', 'uimage1d',
      'uimage1darray', 'uimage2d', 'uimage2darray', 'uimage2dms',
      'uimage2dmsarray', 'uimage2drect', 'uimage3dimagecube', 'uimagebuffer',
      'uimagecube', 'uimagecubearray', 'uint', 'uniform', 'union', 'unsigned',
      'usampler1d', 'usampler1darray', 'usampler2d',
      'usampler2darraysampler2drect', 'usampler2dms', 'usampler2dmsarray',
      'usampler2drect', 'usampler3dsamplercube', 'usamplerbuffer', 'usamplercube',
      'usamplercubearray', 'using', 'usubpassinput', 'usubpassinputms',
      'utexture1d', 'utexture1darray', 'utexture2d', 'utexture2darray',
      'utexture2dms', 'utexture2dmsarray', 'utexture2drect', 'utexture3d',
      'utexturebuffer', 'utexturecube', 'utexturecubearray', 'uvec2', 'uvec3',
      'uvec4', 'varying', 'vec2', 'vec3', 'vec4', 'void', 'volatile', 'while',
      'writeonly'
    ]);

    AddBasicInternalFunctions(['abs', 'sqrt', 'invertsqrt', 'ceil', 'floor',
      'round', 'roundEven', 'trunc', 'fract', 'sign', 'sin', 'cos', 'tan',
      'asin', 'acos', 'atan', 'exp', 'exp2', 'log', 'log2',
      'radians', 'degrees', 'normalize']);
    AddInternalFunctionEx('dot', gtFloat,
      [Argument(gtGenType, 'x'), Argument(gtGenType, 'y')]);
    AddInternalFunctionEx('clamp', gtGenType,
      [Argument(gtGenType, 'x'), Argument(gtGenType, 'minVal'),
       Argument(gtGenType, 'maxVal')]);
    AddInternalFunctionEx('min', gtGenType,
      [Argument(gtGenType, 'x'), Argument(gtGenType, 'y')]);
    AddInternalFunctionEx('max', gtGenType,
      [Argument(gtGenType, 'x'), Argument(gtGenType, 'y')]);
    AddInternalFunctionEx('mix', gtGenType,
      [Argument(gtGenType, 'x'), Argument(gtGenType, 'y'),
      Argument(gtGenType, 'a')]);
    AddInternalFunctionEx('mix', gtGenType,
      [Argument(gtGenType, 'x'), Argument(gtGenType, 'y'),
      Argument(gtFloat, 'a')]);
    AddInternalFunctionEx('mod', gtGenType,
      [Argument(gtGenType, 'x'), Argument(gtGenType, 'y')]);
    AddInternalFunctionEx('mod', gtGenType,
      [Argument(gtGenType, 'x'), Argument(gtFloat, 'y')]);
    AddInternalFunctionEx('pow', gtGenType,
      [Argument(gtGenType, 'x'), Argument(gtGenType, 'y')]);
    AddInternalFunctionEx('step', gtGenType,
      [Argument(gtGenType, 'edge'), Argument(gtGenType, 'x')]);
    AddInternalFunctionEx('smoothstep', gtGenType,
      [Argument(gtGenType, 'edge0'), Argument(gtGenType, 'edge1'),
       Argument(gtGenType, 'x')]);
    AddInternalFunctionEx('smoothstep', gtGenType,
      [Argument(gtFloat, 'edge0'), Argument(gtFloat, 'edge1'),
       Argument(gtGenType, 'x')]);
    AddInternalFunctionEx('cross', gtVec3,
      [Argument(gtVec3, 'x'), Argument(gtVec3, 'y')]);
    AddInternalFunctionEx('distance', gtFloat, [Argument(gtGenType, 'p0'),
      Argument(gtGenType, 'p1')]);
    AddInternalFunctionEx('length', gtFloat, [Argument(gtGenType, 'x')]);

    AddInternalFunctionEx('barrier', gtVoid, []);
    AddInternalFunctionEx('EmitVertex', gtVoid, []);
    AddInternalFunctionEx('EndPrimitive', gtVoid, []);

    AddInternalFunctionEx('interpolateAtSample', gtGenType, [Argument(gtGenType, 'interpolant'), Argument(gtInt, 'sample')]);
    AddInternalFunctionEx('interpolateAtOffset', gtGenType, [Argument(gtGenType, 'interpolant'), Argument(gtVec2, 'offset')]);
    AddInternalFunctionEx('interpolateAtCentroid', gtGenType, [Argument(gtGenType, 'interpolant')]);

    AddInternalFunctionEx('determinant', gtFloat, [Argument(gtMat2, 'm')]);
    AddInternalFunctionEx('determinant', gtFloat, [Argument(gtMat3, 'm')]);
    AddInternalFunctionEx('determinant', gtFloat, [Argument(gtMat4, 'm')]);
    AddInternalFunctionEx('inverse', gtMat2, [Argument(gtMat2, 'm')]);
    AddInternalFunctionEx('inverse', gtMat3, [Argument(gtMat3, 'm')]);
    AddInternalFunctionEx('inverse', gtMat4, [Argument(gtMat4, 'm')]);
    AddInternalFunctionEx('transpose', gtMat2, [Argument(gtMat2, 'm')]);
    AddInternalFunctionEx('transpose', gtMat3, [Argument(gtMat3, 'm')]);
    AddInternalFunctionEx('transpose', gtMat4, [Argument(gtMat4, 'm')]);
    AddInternalFunctionEx('transpose', gtMat2x3, [Argument(gtMat3x2, 'm')]);
    AddInternalFunctionEx('transpose', gtMat2x4, [Argument(gtMat4x2, 'm')]);
    AddInternalFunctionEx('transpose', gtMat3x2, [Argument(gtMat2x3, 'm')]);
    AddInternalFunctionEx('transpose', gtMat3x4, [Argument(gtMat4x3, 'm')]);
    AddInternalFunctionEx('transpose', gtMat4x2, [Argument(gtMat2x4, 'm')]);
    AddInternalFunctionEx('transpose', gtMat4x3, [Argument(gtMat3x4, 'm')]);

    AddInternalFunctionEx('outerProduct', gtMat2, [Argument(gtVec2, 'c'), Argument(gtVec2, 'r')]);
    AddInternalFunctionEx('outerProduct', gtMat3, [Argument(gtVec3, 'c'), Argument(gtVec3, 'r')]);
    AddInternalFunctionEx('outerProduct', gtMat4, [Argument(gtVec4, 'c'), Argument(gtVec4, 'r')]);
    AddInternalFunctionEx('outerProduct', gtMat2x3, [Argument(gtVec3, 'c'), Argument(gtVec2, 'r')]);
    AddInternalFunctionEx('outerProduct', gtMat3x2, [Argument(gtVec2, 'c'), Argument(gtVec3, 'r')]);
    AddInternalFunctionEx('outerProduct', gtMat2x4, [Argument(gtVec4, 'c'), Argument(gtVec2, 'r')]);
    AddInternalFunctionEx('outerProduct', gtMat4x2, [Argument(gtVec2, 'c'), Argument(gtVec4, 'r')]);
    AddInternalFunctionEx('outerProduct', gtMat3x4, [Argument(gtVec4, 'c'), Argument(gtVec3, 'r')]);
    AddInternalFunctionEx('outerProduct', gtMat4x3, [Argument(gtVec3, 'c'), Argument(gtVec4, 'r')]);

    AddInternalFunctionEx('faceforward', gtGenType,
      [Argument(gtGenType, 'N'), Argument(gtGenType, 'I'), Argument(gtGenType, 'Nref')]);
    AddInternalFunctionEx('reflect', gtGenType,
      [Argument(gtGenType, 'I'), Argument(gtGenType, 'N')]);
    AddInternalFunctionEx('refract', gtGenType,
      [Argument(gtGenType, 'I'), Argument(gtGenType, 'N'), Argument(gtFloat, 'eta')]);

    if false {GLSL >= 3.0)} then
      AddBasicInternalFunctions(['sinh', 'cosh', 'tanh', 'asinh', 'acosh',
        'atanh', 'dFdx', 'dFdy', 'dFdxCoarse', 'dFdyCoarse',
        'dFdxFine', 'dFdyFine', 'fwidth', 'fwidthCoarse', 'fwidthFine']);

    InAdd(['all', 'allinvocations', 'allinvocationsequal', 'any', 'anyinvocation',
      'atomicadd', 'atomicand', 'atomiccompswap', 'atomiccounter',
      'atomiccounteradd', 'atomiccounterand', 'atomiccountercompswap',
      'atomiccounterdecrement', 'atomiccounterexchange',
      'atomiccounterincrement', 'atomiccountermax', 'atomiccountermin',
      'atomiccounteror', 'atomiccountersubtract', 'atomiccounterxor',
      'atomicexchange', 'atomicmax', 'atomicmin', 'atomicor', 'atomicxor',
      'bitcount', 'bitfieldextract', 'bitfieldinsert',
      'bitfieldreverse', 'degrees', 'emitstreamvertex', 'endstreamprimitive',
      'equal', 'findlsb', 'findmsb', 'ftransform', 'greaterthan',
      'greaterthanequal', 'groupmemorybarrier',
      'imageatomicadd', 'imageatomicand', 'imageatomiccompswap',
      'imageatomicexchange', 'imageatomicmax', 'imageatomicmin',
      'imageatomicor', 'imageatomicxor', 'imageload', 'imagesamples',
      'imagesize', 'imagestore', 'imulextended',
      'lessthan', 'lessthanequal', 'matrixcompmult',
      'memorybarrier', 'memorybarrieratomiccounter', 'memorybarrierbuffer',
      'memorybarrierimage', 'memorybarriershared', 'noise1', 'noise2', 'noise3',
      'noise4', 'not', 'notequal', 'shadow2d', 'shadow2dproj',
      'shadowld', 'shadowldproj', 'subpassload', 'texelfetch',
      'texelfetchoffset', 'texture', 'texture1d',
      'texture1dlod', 'texture1dproj', 'texture1dprojlod', 'texture2d',
      'texture2dlod', 'texture2dproj', 'texture2dprojlod', 'texture3d',
      'texture3dlod', 'texture3dproj', 'texture3dprojlod', 'texturecube',
      'texturecubelod', 'texturegather', 'texturegatheroffset',
      'texturegatheroffsets', 'texturegrad', 'texturegradoffset', 'texturelod',
      'texturelodoffset', 'textureoffset', 'textureproj', 'textureprojgrad',
      'textureprojgradoffset', 'textureprojlod', 'textureprojlodoffset',
      'textureprojoffset', 'texturequerylevels', 'texturequerylod', 'texturesize',
      'uaddcarry', 'umulextended', 'usubborrow'
    ]);
  end;

end;

procedure TShaderPropEditForm.EditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  MouseCoord: TDisplayCoord;
  UnderLine: Integer;
begin
  inherited;

  // only continue in case of cursor changes
  if (X = FMousePos.X) or (Y = FMousePos.Y) then
    Exit;

  // update last mouse position and restart mouse interval timer
  FMousePos := Point(X, Y);
(*
  FMouseInterval.Enabled := False;
  FMouseInterval.Enabled := True;
*)

  // eventualy invalid old highlighted line
  if FUnderLine > 0 then
    TSynEdit(Sender).InvalidateLine(FUnderLine);

  // check whether word in line should be shown as "link"
  if ssCtrl in Shift then
  begin
    // extract current line
    MouseCoord := TSynEdit(Sender).PixelsToRowColumn(X, Y);
    UnderLine := TSynEdit(Sender).DisplayToBufferPos(MouseCoord).Line;

    // check if line is different to previous line and eventually invalidate
    if UnderLine <> FUnderLine then
    begin
      FUnderLine := UnderLine;
      TSynEdit(Sender).InvalidateLine(FUnderLine);
    end;
  end
  else
    FUnderLine := -1;
end;

procedure TShaderPropEditForm.EditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  Index: Integer;
begin
  for Index := Low(FErrorLines) to High(FErrorLines) do
    if FErrorLines[Index] = Line then
    begin
      BG := $AAAAFF;
      Special := True;
    end;
end;

procedure TShaderPropEditForm.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  NewCaretY: Integer;
begin
  if (scCaretY in Changes) and TSynEdit(Sender).Gutter.Visible  then
  begin
    SetLength(FErrorLines, 0);
    NewCaretY := TSynEdit(Sender).CaretY;
    TSynEdit(Sender).InvalidateGutterLine(FOldCaretY);
    TSynEdit(Sender).InvalidateGutterLine(NewCaretY);
    FOldCaretY := NewCaretY;
  end;
end;

procedure TShaderPropEditForm.OnShaderExprChanged(Sender: TObject);
begin
  CompileShaderButton.Enabled := True;
end;

procedure TShaderPropEditForm.SaveChanges;
begin
  if CompileShaderButton.Enabled then
    CompileShaderButton.OnClick(CompileShaderButton);
  ShaderSynEdit.MarkModifiedLinesAsSaved;
end;

procedure TShaderPropEditForm.ShowError(MsgText: String);
var
  ErrorLines: TStringList;
  Temp: String;
  ClosePos: Integer;
  Index: Integer;
begin
  Splitter.Show;
  SetLength(FErrorLines, 0);

  ErrorLines := TStringList.Create;
  try
    ErrorLines.Text := MsgText;
    for Index := 0 to ErrorLines.Count - 1 do
    begin
      Temp := ErrorLines.Strings[Index];

      // detection for NVIDIA error messages
      if (Length(Temp) >= 4) and (Temp[1] = '0') and (Temp[2] = '(') then
      begin
        ClosePos := Pos(')', Temp);
        if (ClosePos > 0) and (Pos('error', Temp) > 0) then
        begin
          Temp := Copy(Temp, 3, ClosePos - 3);
          SetLength(FErrorLines, Length(FErrorLines) + 1);
          FErrorLines[Length(FErrorLines) - 1] := StrToInt(Temp);
        end;
      end;
    end;
  finally
    ErrorLines.Free;
  end;

  CompileErrorLabel.Caption := MsgText;
  CompileErrorLabel.Hint := MsgText;
  CompileErrorLabel.Show;
  Splitter.Top := ShaderPanel.Height - Splitter.Height - CompileErrorLabel.Height;
end;

procedure TShaderPropEditForm.EditorGutterPaint(Sender: TObject; aLine, X,
  Y: Integer);
var
  SynEdit: TSynEdit;
  num: string;
  numRct: TRect;
  GutterWidth, ImgIndex: Integer;
  OldFont: TFont;
const
  CTickSizes : array [Boolean] of Integer = (2, 5);
begin
  Assert(Sender is TSynEdit);
  SynEdit := TSynEdit(Sender);
  SynEdit.Canvas.Brush.Style := bsClear;
  GutterWidth := SynEdit.Gutter.Width - 4;

  //line numbers
  if (aLine = 1) or (aLine = SynEdit.CaretY) or ((aLine mod 10) = 0) then
  begin
    num := IntToStr(aLine);
    numRct := Rect(x, y, GutterWidth, y + SynEdit.LineHeight);
    OldFont := TFont.Create;
    try
      OldFont.Assign(SynEdit.Canvas.Font);
      SynEdit.Canvas.Font := SynEdit.Gutter.Font;
      SynEdit.Canvas.TextRect(numRct, num, [tfVerticalCenter, tfSingleLine, tfRight]);
      SynEdit.Canvas.Font := OldFont;
    finally
      OldFont.Free;
    end;
  end
  //small tick
  else
  begin
    SynEdit.Canvas.Pen.Color := SynEdit.Gutter.Font.Color;
    numRct.Left := GutterWidth - CTickSizes[(aLine mod 5) = 0];
    numRct.Right := GutterWidth;
    numRct.Top := y + SynEdit.LineHeight div 2;
    SynEdit.Canvas.MoveTo(numRct.Left, numRct.Top);
    SynEdit.Canvas.LineTo(numRct.Right, numRct.Top);
  end;
end;

procedure TShaderPropEditForm.HideError;
begin
  SetLength(FErrorLines, 0);
  CompileErrorLabel.Hide;
  Splitter.Hide;
end;

end.
