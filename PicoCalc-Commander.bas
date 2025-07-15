'----------------------------
' PicoCalc - Commander
'----------------------------
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
Dim copyFile$, copySource$, markMsg$ 

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

    ' Header
    Box 0, 0, MM.HRES, fonth + 20, , lightgray, darkgray
    Font 3
    fontw = MM.FONTWIDTH
    fonth = MM.FONTHEIGHT
    hy = 4
    headline$ = "PICOCALC COMMANDER"
    hx = (MM.HRES - Len(headline$) * fontw) / 2
    Colour red, darkgray
    Print @(hx, hy) headline$

    ' Footer
    Font 7
    fontw = MM.FONTWIDTH
    fonth = MM.FONTHEIGHT
    helptext$ = "F1=Help  ESC=Quit  N=NewFolder  X=Cut  V=Paste"
    helpx = (MM.HRES - Len(helptext$) * fontw) / 2
    helpy = MM.VRES - 2 * fonth
    Colour white, bluebg
    Print @(helpx, helpy) helptext$

    ' Reset font
    Font 1
    fontw = MM.FONTWIDTH
    fonth = MM.FONTHEIGHT
    Colour white, black

    ' Aufruf des Browsers
    a$ = dirwin$(x, y, lines, "B:/", "BAS")

    Select Case a$
      Case "SHOWHELP"
        ShowHelpScreen
        restart = 1
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

=== Subroutine: Help Display ===
Sub ShowHelpScreen
  CLS bluebg
  Colour white, bluebg
  Local txt$(10), j
  txt$(1)  = "  KEY OVERVIEW"
  txt$(2)  = ""
  txt$(3)  = "  up/down   Move cursor"
  txt$(4)  = "  ENTER     Open file/folder"
  txt$(5)  = "  D         Switch drive"
  txt$(6)  = "  F1        Show this help"
  txt$(7)  = "  N         Create folder"
  txt$(8)  = "  DEL       Delete file/folder"
  txt$(9)  = "  X         Mark file for copy"
  txt$(10) = "  V         Paste into folder"
  For j = 1 To 10
    Print @(4, j * fonth + 10) txt$(j)
  Next j
  Do : Loop Until Inkey$ <> ""
  CLS bluebg
End Sub

' === Funktion: Verzeichnis + X/V-Logik ===
Function dirwin$(x, y, lines, path$, ext$)
  Const winw = 39
  Local fname$(128)
  Local i, file$, p$, fcount, top, bottom
  Local ftop, x2, y2, y3, y4
  Local cursor, cmode, endstate
  Local a$, o$, delname$, confirm$, newdir$
  Local k$, code%, ch$, src$, dst$, Line$

  dirwin$  = "RESTART"
  endstate = 1

  x2 = x + 4
  y2 = y + fonth + 3
  y3 = y + 2
  y4 = y2 + 3

  If ext$ = "" Then
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
    ' Read directory contents
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

    ' Display path

  Colour white, RGB(50,50,50)
  Print @(x2, y3) Left$(p$ + Space$(winw), winw)


    ' Liste anzeigen
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

    ' Read key (including arrow handling)
    Do
      k$ = Inkey$
    Loop Until k$ <> ""
    If k$ = Chr$(0) Then
      Do
        k$ = Inkey$
      Loop Until k$ <> ""
    End If
    code%    = Asc(k$)
    ch$      = k$
    endstate = 0

   ' Special keys
    Select Case code%
      Case 128  ' ↑
        If cursor > 1 Then
          cursor = cursor - 1
          endstate = 2
        ElseIf ftop > 1 Then
          ftop = ftop - 1
          endstate = 3
        End If

      Case 129  ' ↓
        If (cursor + ftop - 1) < fcount Then
          If cursor < lines Then
            cursor = cursor + 1
            endstate = 4
          Else
            ftop = ftop + 1
            endstate = 2
          End If
        End If

      Case 13   ' ENTER
        a$ = fname$(cursor+ftop-1)
        o$ = Mid$(a$,2)
        If Left$(a$,1) = "1" Then
          endstate = 1
          If o$ = ".." Then
            On Error Skip
            Chdir ".."
            p$ = Cwd$
          Else
            If Right$(p$,1) <> "/" Then p$ = p$ + "/"
            p$ = p$ + o$
            On Error Skip
            Chdir p$
            If MM.Errno Then Exit Function
          End If
        Else
          dirwin$ = p$ + "$" + o$
          Exit Function
        End If

      Case 27   ' ESC
        CLS
        dirwin$ = "QUIT"
        Exit Function

      Case 127  ' DEL
        delname$ = Mid$(fname$(cursor+ftop-1),2)
        Colour white, red
        Print @(x2, y3) "Delete? Y/N"
        Do
          confirm$ = Inkey$
        Loop Until confirm$ <> ""
        If UCase$(confirm$) = "Y" Then Kill delname$
        dirwin$ = "RESTART"
        Exit Function



      Case 145  ' F1
        dirwin$ = "SHOWHELP"
        Exit Function
    End Select

    Select Case UCase$(ch$)
    
          Case "N"  
        newdir$ = ""
        Colour white, RGB(50,50,50)
        Print @(x2, y3) "New folder: ";
        Input newdir$
        If newdir$ <> "" Then Mkdir newdir$
        dirwin$ = "RESTART"
        Exit Function
    
      Case "D"
        If Left$(p$,1) = "A" Then Drive "B:" Else Drive "A:"
        p$ = Cwd$
        endstate = 1

      Case "X"
        copyFile$   = Mid$(fname$(cursor+ftop-1),2)
        copySource$ = p$
        Colour yellow, darkgray
        Print @(x2, y3) "Marked: " + copyFile$, "V for insert"
          Do
    k$ = Inkey$
  Loop Until k$ <> ""

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
            Close #1
            Close #2
            Kill src$
          End If
        End If
        copyFile$   = ""
        copySource$ = ""
        
        endstate = 1
        'dirwin$     = "RESTART"
        'Exit Function
    End Select

  Loop
  ' Fallback 
  dirwin$ = "RESTART"
End Function