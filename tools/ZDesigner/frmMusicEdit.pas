unit frmMusicEdit;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.CheckLst, frmCompEditBase, ZClasses,
  AudioComponents, DesignerGUI;

type
  TMusicEditFrame = class(TCompEditFrameBase)
    PlayButton: TButton;
    Memo1: TMemo;
    procedure PlayButtonClick(Sender: TObject);
  private
    Music : TMusic;
    IsPlaying : boolean;
//    Thread : TThread;
  public
    procedure SetComponent(C : TZComponent; TreeNode : TZComponentTreeNode); override;
    procedure OnEditorClose; override;
  end;

implementation

{$R *.dfm}

uses
  AudioPlayer, ZPlatform;

type
  TPlaybackThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Music : TMusic;
  end;

{ TPlaybackThread }

procedure TPlaybackThread.Execute;
var
  LastTime,TimeStep,Now : single;
begin
  LastTime := Platform_GetTime;
  Music.Start;
  while not Terminated do
  begin
    Now := Platform_GetTime;
    TimeStep := (Now - LastTime);
    LastTime := Now;
    Music.AdvanceMusic(TimeStep);
    AudioPlayer.EmitSoundsInEmitList;
    Sleep(10);
  end;
end;

procedure TMusicEditFrame.SetComponent(C: TZComponent; TreeNode : TZComponentTreeNode);
begin
  inherited;
  Music := (C as TMusic);
end;

procedure TMusicEditFrame.PlayButtonClick(Sender: TObject);
//var
//  T :TPlaybackThread;
begin
  if IsPlaying then
  begin
    PlayButton.Caption := '&Play music';
//    PlayTimer.Enabled := False;
    Music.Stop;
    AudioComponents.CurrentMusic := nil;
    DesignerStopAllAudio;
//    if Thread<>nil then
//    begin
//      Thread.Terminate;
//      Thread := nil;
//    end;
  end
  else
  begin
    Platform_InitGlobals; //reset timer
    PlayButton.Caption := '&Stop music';
    Music.Start;
    AudioComponents.CurrentMusic := Music;
//      T := TPlaybackThread.Create(True);
//      T.Priority := tpTimeCritical;
//      T.Music:=Music;
//      T.Start;
//      T.FreeOnTerminate := True;
//      Self.Thread := T;
  end;
  IsPlaying := not IsPlaying;
end;

procedure TMusicEditFrame.OnEditorClose;
begin
  if IsPlaying then
    PlayButton.Click;
  Music := nil;
end;


end.
