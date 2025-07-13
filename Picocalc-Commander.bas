'----------------------------
' PicoCalc - Commander
'----------------------------
Clear
Option BASE 1
Option EXPLICIT

' === Constants ===
Const launcherPath$ = "B:/Picocalc-Commander.bas"

' === Color and font definitions ===
Dim fontw = MM.FONTWIDTH
Dim fonth = MM.FONTHEIGHT
Dim bluebg = RGB(0, 0, 200)
Dim white = RGB(255, 255, 255)
Dim black = RGB(0, 0, 0)
Dim red   = RGB(255, 0, 0)
Dim lightgray = RGB(200, 200, 200)
Dim darkgray  = RGB(64, 64, 64)
Dim darkblue = RGB(10, 10, 100)

' === Launch File Manager ===
Sub StartFileManager
  Local a$, f$
  Const winw = 39
  Const lines = 20
  Local x = 0
  Local y = fonth + 24

  CLS bluebg

  ' --- Header background box ---
  Box 0, 0, MM.HRES, fonth + 20, , lightgray, darkgray

  ' --- Centered header title ---
  Font 3
  fontw = MM.FONTWIDTH
  fonth = MM.FONTHEIGHT
  Local hy = 4
  Local headline$ = "PICOCALC COMMANDER"
  Local hx = (MM.HRES - Len(headline$) * fontw) / 2
  Colour red, darkgray
  Print @(hx, hy) headline$

  ' --- Help bar at the bottom ---
  Font 7
  fontw = MM.FONTWIDTH
  fonth = MM.FONTHEIGHT
  Local helptext$ = " F1 = Help  ESC = Quit"
  'Local helpx = (MM.HRES - Len(helptext$) * fontw) / 2
  Local helpx = 0
  Local helpy = MM.VRES - 2 * fonth
  Colour white, bluebg
  Local freeBytes = Int(MM.INFO(FREE SPACE) / 1048576)
  Print @(helpx, helpy) helptext$, "  =(^.^)=  ", freeBytes, "MB free"

  ' --- Reset to default font ---
  Font 1
  fontw = MM.FONTWIDTH
  fonth = MM.FONTHEIGHT

  ' --- Start the file browser window ---
  Colour white, black
  a$ = dirwin$(x, y, lines, "B:/", "BAS")
  If a$ = "SHOWHELP" Then
    ShowHelpScreen
    RUN launcherPath$
  EndIf

  ' --- Run selected .BAS file if applicable ---
  If a$ <> "" And Instr(a$, "$") > 0 Then
    f$ = Mid$(a$, Instr(a$, "$") + 1)
    If UCase$(Right$(f$, 4)) = ".BAS" Then
      RUN f$
    EndIf
  EndIf
End Sub

' === File Browser Window Function ===
Function dirwin$(x, y, lines, path$, ext$)
  Const winw = 39
  Local fname$(256)
  Local i, file$, p$, fcount, top, bottom
  Local ftop, x2, y2, y3, y4
  Local cursor, cmode, endstate
  Local a$, o$, drv$
  Local prefix$
  Local showpath$
  dirwin$ = "999"
  endstate = 1

  x2 = x + 4
  y2 = y + fonth + 3
  y3 = y + 2
  y4 = y2 + 3

  ' --- Extension filtering ---
  If ext$ = "" Then
    ext$ = "*"
  Else
    ext$ = "*." + UCase$(ext$)
  EndIf

  ' --- Determine working path ---
  If path$ <> "" Then
    p$ = path$
  Else
    p$ = Cwd$
  EndIf

  ' --- Attempt to switch directory ---
  On Error Skip
  Chdir p$
  If MM.Errno Then
    p$ = "A:/"
    Drive "A:"
  EndIf

  Do
    If endstate = 1 Then
      ' --- Refresh file listing ---
      fcount = 0
      cmode = 0
      cursor = 1
      ftop = 1
      If Len(p$) > 3 Then
        fcount = 1
        fname$(1) = "1.."
      EndIf

      ' --- Load directory names ---
      On Error Skip 9
      file$ = Dir$("*", DIR)
      Do While file$ <> "" And fcount < 256
        If Left$(file$, 1) <> "." Then
          fcount = fcount + 1
          fname$(fcount) = "1" + file$
        EndIf
        file$ = Dir$()
      Loop

      ' --- Load file names ---
      file$ = Dir$(ext$, FILE)
      Do While file$ <> "" And fcount < 256
        If Left$(file$, 1) <> "." Then
          fcount = fcount + 1
          fname$(fcount) = "2" + file$
        EndIf
        file$ = Dir$()
      Loop

      If MM.Errno Then Exit Function

      ' --- Prepare path line ---
      'Colour white, RGB(75, 75, 75)
      Print @(x2, y3) Space$(winw)

      ' --- Add drive symbol to path ---
      If Left$(p$, 2) = "A:" Then
        prefix$ = Chr$(168) + " "
      ElseIf Left$(p$, 2) = "B:" Then
        prefix$ = Chr$(153) + " "
      Else
        prefix$ = ""
      EndIf

      ' --- Trim path with ellipsis if needed ---
      If Len(prefix$ + p$) > winw Then
        showpath$ = prefix$ + Left$(p$, winw - Len(prefix$) - 3)
        showpath$ = showpath$ + "..."
      Else
        showpath$ = prefix$ + p$
      EndIf
    EndIf

    ' --- Display the path line ---
    Colour white, black
    Print @(x2, y3) Left$(showpath$ + Space$(winw), winw)

    Colour white, black

    Do
      ' --- Set scroll area ---
      Select Case endstate
        Case 1, 2
          top = ftop
          bottom = ftop + lines - 1
        Case 3
          top = ftop + cursor - 1
          bottom = ftop + lines - 1
        Case 4
          top = ftop + cursor - 2
          bottom = top + 1
      End Select

      ' --- Display file list ---
      For i = top To bottom
        If i <= fcount Then
          a$ = Mid$(fname$(i), 2)
          file$ = Left$(a$ + Space$(winw - 5), winw - 5)
          If Left$(fname$(i), 1) = "1" Then
            file$ = file$ + "DIR  "
          Else
            file$ = file$ + "FILE "
          EndIf
          If i = ftop - 1 + cursor Then
            cmode = 2
          Else
            cmode = 0
          EndIf
        Else
          file$ = Space$(winw)
          cmode = 0
        EndIf
        Print @(x2, y4 + (i - ftop) * fonth, cmode) file$
      Next i

      ' --- Wait for key press ---
      Do : a$ = Inkey$ : Loop Until a$ <> ""
      i = Asc(a$)
      endstate = 0

      ' --- Key handling ---
      If i = 128 Then
        If cursor > 1 Then
          cursor = cursor - 1 : endstate = 2
        ElseIf ftop > 1 Then
          ftop = ftop - 1 : endstate = 3
        EndIf
      ElseIf i = 129 And (cursor + ftop - 1) < fcount Then
        If cursor >= lines Then
          ftop = ftop + 1 : endstate = 2
        Else
          cursor = cursor + 1 : endstate = 4
        EndIf
      ElseIf i = 13 Then
        a$ = fname$(cursor + ftop - 1)
        o$ = Mid$(a$, 2)
        If Left$(a$, 1) = "1" Then
          endstate = 1
          If o$ = ".." Then
            On Error Skip : Chdir ".." : p$ = Cwd$
          Else
            If Right$(p$, 1) <> "/" Then p$ = p$ + "/"
            p$ = p$ + o$
            On Error Skip : Chdir p$ : If MM.Errno Then Exit Function
          EndIf
        Else
          endstate = 5
        EndIf
      ElseIf i = 127 Then
        ' --- Delete file/folder ---
        Local delname$ = Mid$(fname$(cursor + ftop - 1), 2)
        Local confirm$
        Colour white, red
        Print @(x2, y3) Space$(winw)
        Print @(x2, y3) "Delete file/folder? Y/N"
        Do : confirm$ = Inkey$ : Loop Until confirm$ <> ""
        If UCase$(confirm$) = "Y" Then Kill delname$
        RUN launcherPath$
      ElseIf i = 146 Then
        ' --- Create new folder ---
        Local newdir$
        Colour white, RGB(50, 50, 50)
        Print @(x2, y3) Space$(winw)
        Print @(x2, y3) "New folder: ";
        Input newdir$
        If newdir$ <> "" Then Mkdir newdir$
        RUN launcherPath$
      ElseIf i = 145 Then
        ' --- Show help screen ---
        dirwin$ = "SHOWHELP" : Exit Function
      ElseIf UCase$(a$) = "D" Then
        ' --- Switch drive ---
        endstate = 1
        If Left$(p$, 1) = "A" Then drv$ = "B:" Else drv$ = "A:"
        On Error Skip : Drive drv$ : p$ = Cwd$
        Exit Do
      ElseIf i = 27 Then
        ' --- Exit program ---
        CLS : dirwin$ = "" : Exit Function
      EndIf

    Loop Until endstate > 0
  Loop Until endstate > 4

  ' --- Return selected file path ---
  dirwin$ = p$ + "$" + o$
End Function

' === Help Screen ===
Sub ShowHelpScreen
  CLS bluebg
  Colour white, bluebg
  Local txt$(9)
  txt$(1) = "  KEY OVERVIEW"
  txt$(2) = ""
  txt$(3) = "  up/down   Move cursor"
  txt$(4) = "  ENTER     Open file/folder"
  txt$(5) = "  D         Switch drive"
  txt$(6) = "  F1        Show this help"
  txt$(7) = "  F2        Create folder"
  txt$(8) = "  DEL       Delete file/folder"
  txt$(9) = "  ESC       Exit"
  Local i
  For i = 1 To 9
    Print @(4, i * fonth + 10) txt$(i)
  Next i
  Do : Loop Until Inkey$ <> ""
  CLS bluebg
End Sub

' === Start the File Manager ===
StartFileManager