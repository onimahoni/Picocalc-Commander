'----------------------------1
' PicoCalc - Commander
'----------------------------
'Code base: Questarians Utility library'
' https://forum.clockworkpi.com/t/building-an-mmbasic-utility-library/18496'

Clear
Option BASE 1
Option EXPLICIT

' --- Global Variables ---
Dim fontw = MM.FONTWIDTH
Dim fonth = MM.FONTHEIGHT
Dim bluebg = RGB(0, 0, 200)
Dim white = RGB(255, 255, 255)
Dim black = RGB(0, 0, 0)
Dim red   = RGB(255, 0, 0)
Dim lightgray = RGB(200, 200, 200)
Dim darkgray  = RGB(64, 64, 64)
Dim yellow = RGB(255, 255, 0)
Dim copyFile$, copySource$
Dim currentPath$ = "B:/"
Dim fname$(128)
Dim currentExt$ = "BAS"
Dim copyMode$
Dim prefix$

' === Start Commander ===
StartCommander

' === Subroutine: Commander ===
Sub StartCommander
  Const winw = 39, lines = 20
  Local restart, a$, f$
  Local x = 0, y = fonth + 24
  Local hy, hx, helptext$, helpx, helpy, headline$

  Do
    restart = 0
    CLS bluebg

    Box 0, 0, MM.HRES, fonth + 20, , lightgray, darkgray
    Font 3
    fontw = MM.FONTWIDTH
    fonth = MM.FONTHEIGHT
    hy = 4
    headline$ = "PICOCALC COMMANDER"
    hx = (MM.HRES - Len(headline$) * fontw) / 2
    Colour red, darkgray
    Print @(hx, hy) headline$

    Font 7
    fontw = MM.FONTWIDTH
    fonth = MM.FONTHEIGHT
    helptext$ = "F1=Help    ESC=Quit"
    helpx = (MM.HRES - Len(helptext$) * fontw) / 2
    helpy = MM.VRES - 2 * fonth
    Colour white, bluebg
    Print @(helpx, helpy) helptext$

    Font 1
    fontw = MM.FONTWIDTH
    fonth = MM.FONTHEIGHT
    Colour white, black

    a$ = dirwin$(x, y, lines, currentPath$, currentExt$)
    If currentExt$ <> "*" And currentExt$ <> "BAS" Then currentExt$ = "BAS"
    

    Select Case a$
      Case "SHOWHELP"
        ShowHelpScreen
        restart = 1
           If currentExt$ <> "*" And currentExt$ <> "BAS" Then currentExt$ = "BAS"     
          Case "RESTART"
        restart = 1
      Case ""            
        restart = 1
      Case Else          
        If Instr(a$, "$") > 0 Then
          f$ = Mid$(a$, Instr(a$, "$") + 1)
          If UCase$(Right$(f$,4)) = ".BAS" Then
            RUN f$
          End If
        End If
    End Select
  Loop While restart = 1
End Sub

'=== Subroutine: Help Display ===
Sub ShowHelpScreen
  CLS bluebg
  Colour white, bluebg
  Local txt$(15), j
  txt$(1)  = "  KEY OVERVIEW"
  txt$(2)  = ""
  txt$(3)  = "  up/down   Move cursor"
  txt$(4)  = "  ENTER     Open file/folder"
  txt$(5)  = "  D         Switch drive"
  txt$(6)  = "  F1        Show this help"
  txt$(7)  = "  N         Create folder"
  txt$(8)  = "  DEL       Delete file/folder"
  txt$(9)  = "  C         Copy file"
  txt$(10)  = "  X         Cut file"
  txt$(11) = "  V         Paste into folder"
  txt$(12) = "  R         Rename Folder"
  txt$(13) = "  F         Filter all/.bas"
  txt$(14) = "  "
  txt$(15) = "  Version   0.14 "
  
  For j = 1 To 15
    Print @(4, j * fonth + 10) txt$(j)
  Next j
  Do : Loop Until Inkey$ <> ""
  CLS bluebg
  If currentExt$ = "" Then currentExt$ = "BAS"
End Sub

 Sub DeleteFolderRecursive(folder$)
  Local f$, full$, d$(100), i%, count%

  ' Sicherheitsprüfung: nicht aktuelles Verzeichnis löschen
  If UCase$(folder$) = UCase$(Cwd$) Then
    Print "Cannot delete current directory!"
    Exit Sub
  End If

  ' Trailing Slash entfernen
  If Right$(folder$, 1) = "/" Then folder$ = Left$(folder$, Len(folder$) - 1)


  f$ = Dir$(folder$ + "/*", FILE)
  Do While f$ <> ""
    full$ = folder$ + "/" + f$
    On Error Ignore
    Kill full$
    On Error Abort
    f$ = Dir$()
  Loop

  count% = 0
  f$ = Dir$(folder$ + "/*", DIR)
  Do While f$ <> ""
    If f$ <> "." And f$ <> ".." Then
      count% = count% + 1
      d$(count%) = folder$ + "/" + f$
    End If
    f$ = Dir$()
  Loop


  For i% = 1 To count%
    DeleteFolderRecursive d$(i%)
  Next i%

  On Error Ignore
  Rmdir folder$
  On Error Abort
End Sub

Sub ShowBMP(filename$)
  CLS
  If Dir$(filename$, FILE) <> "" Then
    Load Image filename$, 0, 0
    Do : Loop Until Inkey$ <> ""
  Else
    Colour red, black
    Print "Not a valid BMP file!"
    Pause 1000
  End If
  CLS
End Sub

Sub ShowTextFile(filename$)
  Local line$, i, txt$(20)
  Local f%

  CLS black
  Colour white, black

  Open filename$ For Input As #1
  i = 1
  Do While Not EOF(1) And i <= 20
    Line Input #1, line$
    txt$(i) = Left$(line$, 38) ' max. Zeichen pro Zeile
    i = i + 1
  Loop
  Close #1

  For f% = 1 To i - 1
    Print @(4, f% * fonth + 10) txt$(f%)
  Next f%

  Do : Loop Until Inkey$ <> ""
  CLS bluebg
End Sub

Function dirwin$(x, y, lines, path$, ext$)
  Const winw = 39
  
  Local i, file$, p$, fcount, top, bottom
  Local ftop, x2, y2, y3, y4
  Local cursor, cmode, endstate
  Local a$, o$, delname$, confirm$, newdir$
  Local k$, code%, ch$, src$, dst$, line$, renameSrc$, renameDst$, renameBuf$

  dirwin$  = "RESTART"
  endstate = 1

  x2 = x + 4
  y2 = y + fonth + 3
  y3 = y + 2
  y4 = y2 + 3

If ext$ = "" Or ext$ = "*" Then
  ext$ = "*"
Else
  ext$ = "*." + UCase$(ext$)
End If

  If path$ <> "" Then
    p$ = path$
  Else
    p$ = Cwd$
  End If

  On Error Skip
  Chdir p$
  If MM.Errno Then
    p$ = "A:/"
    Drive "A:"
  End If

  Do
    If endstate = 1 Then
      fcount = 0 : cursor = 1 : ftop = 1
      If Len(p$) > 3 Then
        fcount = 1
        fname$(1) = "1.."
      End If
      On Error Skip 9
      file$ = Dir$("*", DIR)
      Do While file$ <> "" And fcount < 128
        If Left$(file$,1) <> "." Then
          fcount = fcount + 1
          fname$(fcount) = "1" + file$
        End If
        file$ = Dir$()
      Loop
      file$ = Dir$(ext$, FILE)
      Do While file$ <> "" And fcount < 128
        If Left$(file$,1) <> "." Then
          fcount = fcount + 1
          fname$(fcount) = "2" + file$
        End If
        file$ = Dir$()
      Loop
    End If

    If Left$(p$,2) = "A:" Then
      prefix$ = Chr$(168) + " "
    ElseIf Left$(p$,2) = "B:" Then
      prefix$ = Chr$(153) + " "
    Else
      prefix$ = ""
    End If
    Colour white, darkgray
    Print @(x2, y3) Left$(prefix$ + p$ + Space$(winw), winw)

    Colour white, black
    top    = ftop
    bottom = ftop + lines - 1
    For i = top To bottom
      If i <= fcount Then
        a$    = Mid$(fname$(i),2)
        file$ = Left$(a$ + Space$(winw-5), winw-5)
        If Left$(fname$(i),1) = "1" Then
          file$ = file$ + "DIR  "
        Else
          file$ = file$ + "FILE "
        End If
        If i = ftop-1 + cursor Then
          cmode = 2
        Else
          cmode = 0
        End If
      Else
        file$ = Space$(winw)
        cmode = 0
      End If
      Print @(x2, y4 + (i-ftop)*fonth, cmode) file$
    Next i

    Do
      k$ = Inkey$
    Loop Until k$ <> ""
    If k$ = Chr$(0) Then
      Do : k$ = Inkey$ : Loop Until k$ <> ""
    End If
    code%    = Asc(k$)
    ch$      = k$
    endstate = 0

    Select Case code%
      Case 128
        If cursor > 1 Then
          cursor = cursor - 1
          endstate = 2
        ElseIf ftop > 1 Then
          ftop = ftop - 1
          endstate = 3
        End If
        
      Case 129
        If (cursor + ftop - 1) < fcount Then
          If cursor < lines Then
            cursor = cursor + 1
            endstate = 4
          Else
            ftop = ftop + 1
            endstate = 2
          End If
        End If
        
Case 13
  a$ = fname$(cursor + ftop - 1)
  o$ = Mid$(a$, 2)
  If Left$(a$,1) = "1" Then
    ' Ordner öffnen
    endstate = 1
    If o$ = ".." Then
      On Error Skip
      Chdir ".."
      p$ = Cwd$
      currentPath$ = p$
    Else
      If Right$(p$,1) <> "/" Then p$ = p$ + "/"
      p$ = p$ + o$
      On Error Skip
      Chdir p$
      currentPath$ = p$
      If MM.Errno Then Exit Function
    End If
  Else
    If UCase$(Right$(o$, 4)) = ".BMP" Then
      ShowBMP o$
      Exit Function
    ElseIf UCase$(Right$(o$, 4)) = ".WAV" Then
      Play WAV o$
      Do : Loop Until Inkey$ <> ""
      Exit Function
    ElseIf UCase$(Right$(o$, 4)) = ".TXT" Then
      ShowTextFile o$
      Exit Function
    ElseIf UCase$(Right$(o$, 4)) = ".BAS" Then
      dirwin$ = p$ + "$" + o$
      currentPath$ = p$
      Exit Function
    End If
  End If
        
      Case 27
        CLS
        dirwin$ = "QUIT"
        Exit Function
        
      Case 127
        delname$ = Mid$(fname$(cursor+ftop-1),2)
        Colour white, red
        Print @(x2, y3) "Delete? Y/N"
        Do : confirm$ = Inkey$ : Loop Until confirm$ <> ""
        If UCase$(confirm$) = "Y" Then
          If Left$(fname$(cursor+ftop-1),1) = "1" Then
            ' Ordner löschen (rekursiv)
            DeleteFolderRecursive delname$
          Else
            ' Datei löschen
            Kill delname$
          End If
        End If
        currentPath$ = p$
        dirwin$ = "RESTART"
        Exit Function
        
Case 145  ' F1
        dirwin$ = "SHOWHELP"
        Exit Function
    End Select

    Select Case UCase$(ch$)
      Case "N"
        newdir$ = ""
        Colour white, darkgray
        Print @(x2, y3) "New folder: ";
        Input newdir$
        If newdir$ <> "" Then Mkdir newdir$
        currentPath$ = p$
        dirwin$ = "RESTART"
        Exit Function

      Case "D"
        If Left$(p$,1) = "A" Then Drive "B:" Else Drive "A:"
        p$ = Cwd$
        endstate = 1

          Case "C"
        If Left$(fname$(cursor+ftop-1),1) = "1" Then
          Colour red, darkgray
          Print @(x2, y3) "Cannot copy a folder!"
          Do : k$ = Inkey$ : Loop Until k$ <> ""
          endstate = 1
        Else
          copyFile$   = Mid$(fname$(cursor+ftop-1),2)
          copySource$ = p$
          copyMode$   = "COPY"
          Colour yellow, darkgray
          Print @(x2, y3) "copy file: " + copyFile$
          Do : k$ = Inkey$ : Loop Until k$ <> ""
        End If
        
      Case "X"
        If Left$(fname$(cursor+ftop-1),1) = "1" Then
          Colour red, darkgray
          Print @(x2, y3) "Cannot cut a folder!"
          Do : k$ = Inkey$ : Loop Until k$ <> ""
          endstate = 1
        Else
          copyFile$   = Mid$(fname$(cursor+ftop-1),2)
          copySource$ = p$
          copyMode$   = "CUT"
          Colour yellow, darkgray
          Print @(x2, y3) "cut file: " + copyFile$
          Do : k$ = Inkey$ : Loop Until k$ <> ""
        End If

      Case "V"
        If copyFile$ <> "" Then
          If Right$(copySource$,1) <> "/" Then copySource$ = copySource$ + "/"
          If Right$(p$,1) <> "/" Then p$ = p$ + "/"
          src$ = copySource$ + copyFile$
          dst$ = p$ + copyFile$
          If UCase$(src$) <> UCase$(dst$) Then
            Open src$ For Input As #1
            Open dst$ For Output As #2
            Do While Not EOF(1)
              Line Input #1, line$
              Print #2, line$
            Loop
            Close #1 : Close #2
            If copyMode$ = "CUT" Then Kill src$
          End If
        End If
        copyFile$ = "" : copySource$ = "" : copyMode$ = "" : endstate = 1

      Case "R"
        renameSrc$ = Mid$(fname$(cursor + ftop - 1), 2)
        If Right$(p$,1) <> "/" Then p$ = p$ + "/"
        Colour white, darkgray
        Print @(x2, y3) "New name:";
        Input renameDst$, x2 + 80, y3
        renameDst$ = p$ + renameDst$
        If renameDst$ <> "" Then
          If Dir$(renameSrc$) <> "" Then
            Open renameSrc$ For Input As #1
            Open renameDst$ For Output As #2
            Do While Not EOF(1)
              Line Input #1, renameBuf$
              Print #2, renameBuf$
            Loop
            Close #1 : Close #2 : Kill renameSrc$
          EndIf
        EndIf
        dirwin$ = "RESTART"
        currentPath$ = p$
        Exit Function
    
    
Case "F"
  If currentExt$ = "*" Then
    currentExt$ = "BAS"
  Else
    currentExt$ = "*"
  End If
  currentPath$ = p$
  dirwin$ = "RESTART"
  Exit Function

End Select
  Loop
  dirwin$ = "RESTART"
End Function


