unit LoginFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBCtrls, IniFiles,
  dDatenbank, DB, UniProvider;

const
  C_INI_FILE = 'user.ini';
  C_SECTION  = 'Login';

type

  { TLoginForm }

  TLoginForm = class(TForm)
    btnLogin:          TButton;
    btnRegister:       TButton;
    cbkStayLoggedIn:   TCheckBox;
    edtUsername:       TEdit;
    edtUserPassword:   TEdit;
    lblLoginFormTitle: TLabel;
    lblLoginStatusMsg: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate   (Sender: TObject);
    procedure ConnectDB;
    procedure btnLoginClick(Sender: TObject);

  private
    FLoginSuccessful: Boolean;
    function LoadStayLoggedIn: string;
    function ValidateUserCredentials(username, password: string): Boolean;
  public
    function Execute : boolean;
    procedure ReadSettings;
    procedure WriteSettings;
    property LoginSuccessful: Boolean read FLoginSuccessful;
  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.lfm}


{ TLoginForm }
{Connects DB to LoginForm Form}
procedure TLoginForm.ConnectDB;
begin
  if not Assigned(dmMain) then
  begin
    ShowMessage('Data module (dmMain) not assigned.');
    Exit;
  end;
  try
    if not dmMain.cDatenbank.Connected then
           dmMain.cDatenBank.Connected := True;
    if not dmMain.qUsers.Active then
           dmMain.qUsers.Open;
    WriteLn('Database connected successfully!');

           LoadStayLoggedIn;
  Except
    on E: Exception do
    ShowMessage('Database Error: ' + E.Message);
  end;
end;

{Loads users saved data from iniFile if stay logged in is checked. before window appears}
function TLoginForm.LoadStayLoggedIn: string;
var
  Sett: TIniFile;
begin
  Sett := TIniFile.Create(C_INI_FILE);
  try
    Result := Sett.ReadString(C_SECTION, 'Username', '');
  finally
    Sett.Free;
  end;
end;

{Reads all saved settings and updates form}
procedure TLoginForm.ReadSettings;
var
  Sett: TIniFile;
begin
  Sett := TIniFile.Create(C_INI_FILE);
  try
    edtUsername.Text := Sett.ReadString(C_SECTION, 'Username', '');
    cbkStayLoggedIn.Checked := Sett.ReadBool(C_SECTION, 'StayLoggedIn', False);
  finally
    Sett.Free;
  end;
end;

{Save username for future LoginForm}
procedure TLoginForm.WriteSettings;
var
  Sett: TIniFile; //var Sett for class TIniFile
begin
  Sett := TIniFile.Create(C_INI_FILE);   //this is the TIniFile OBJ being created
  try
    //Stores current username and checkbox state
    Sett.WriteString(C_SECTION, 'Username', edtUsername.Text);
    Sett.WriteString(C_SECTION, 'Password', edtUserPassword.Text);
    Sett.WriteBool(C_SECTION, 'StayLoggedIn', cbkStayLoggedIn.Checked);
    WriteLn('INI file saved successfully!');
  finally
    Sett.Free;
  end;
end;


{Creats the form and initilizes users saved data to load before window appears}
procedure TLoginForm.FormCreate(Sender: TObject);
var
  savedUser: string;
begin
  ConnectDB;
  savedUser := LoadStayLoggedIn;

  if savedUser <> '' then
  begin
    WriteLn('Found saved user: ' + savedUser);
    edtUsername.Text := savedUser;

    //btnLoginClick(nil);
  end
  else
    WriteLn('No saved user found.');
end;

procedure TLoginForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    //LoginForm := nil;
end;

{LoginForm Button Click}
procedure TLoginForm.btnLoginClick(Sender: TObject);
var
  username, password: string;
begin
  // Get username and password from the form
  username := Trim(edtUsername.Text);
  password := Trim(edtUserPassword.Text);


  WriteLn('Attempting login with Username: ', username);
  WriteLn('Entered Password: ', password);

  // Call ValidateUserCredentials function to check if credentials are correct
  if ValidateUserCredentials(username, password) then
  begin
    FLoginSuccessful := True;
    lblLoginStatusMsg.Caption := 'Logging in...';
    WriteLn('Login successful for user: ' + username);

    // If StayLoggedIn checkbox is checked, save settings
    if cbkStayLoggedIn.Checked then
    begin
      lblLoginStatusMsg.Caption := lblLoginStatusMsg.Caption + ' (Will stay logged in)';

      WriteSettings;  // Save username and checkbox state
    end;
     WriteLn('closing..');
    Close;  // Close the form and return True
     WriteLn('closed.');
  end
  else
  begin
    FLoginSuccessful := False;
    lblLoginStatusMsg.Caption := 'Invalid username or password.';
    WriteLn('Login failed: invalid credentials for user: ' + username);
  end;
end;

{Validate entered username and password}
function TLoginForm.ValidateUserCredentials(username, password: string): Boolean;
var
  storedPasswordHash: string;
begin
  Result := False;

  // Query the database to get the stored password hash for the username
  try
    dmMain.qUsers.ParamByName('username').AsString := username;
    dmMain.qUsers.Open; // Open the query to execute it

    // Check if user exists
    if dmMain.qUsers.IsEmpty then
    begin
      ShowMessage('User not found');
      Exit;
    end;

    // Get the password hash from the query result
    storedPasswordHash := dmMain.qUsers.FieldByName('PWDHASH').AsString;

    if storedPasswordHash = password then
      Result := True
    else
      ShowMessage('Invalid password');
  except
    on E: Exception do
      ShowMessage('Error validating user: ' + E.Message);
  end;
end;

{shows form modally and returns success/failure}
function TLoginForm.Execute: Boolean;
begin
  dmMain.cDatenbank.Connected := True;
  FLoginSuccessful := False;
  ShowModal;
  Result := FLoginSuccessful;
end;

end.

