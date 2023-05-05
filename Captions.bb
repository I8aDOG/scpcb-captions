Global CaptionFilePath$ = "Data\captions.ini"
Global CaptionFile$

Global Font1Bold%, Font1Italic%, Font1BoldItalic%
Global CaptionTextHeight%
Global CaptionCurrentBoxTop#
Global CaptionCurrentBoxHeight#

Type CaptionMsg
    field soundPath$
    field sound.Sound

    field yPos#
    field curYPos#

    field txt$

    field r%, g%, b%
    field isItalic%
    field isBold%
    field isGlitch%

    field timeLeft#

    field alpha#
End Type

Type QueuedCaptionMsg
    field caption.CaptionMsg

    field timeStart#

    field soundPath$
    field sound.Sound

    field txt$

    field r%, g%, b%
    field isItalic%
    field isBold%
    field isGlitch%

    field timeLeft#
End Type

Type CaptionColor
    field name$

    field r%
    field g%
    field b%
End Type

Function LoadCaptionFonts()
    Font1Bold = AALoadFont("GFX\font\cour\Courier New.ttf", Int(19 * (GraphicHeight / 1024.0)), 1,0,0)
    Font1Italic = AALoadFont("GFX\font\cour\Courier New.ttf", Int(19 * (GraphicHeight / 1024.0)), 0,1,0)
    Font1BoldItalic = AALoadFont("GFX\font\cour\Courier New.ttf", Int(19 * (GraphicHeight / 1024.0)), 1,1,0)

    CaptionTextHeight = Int(19 * (GraphicHeight / 1024.0))*1.5
End Function

Function UpdateCaptions()
    For queue.QueuedCaptionMsg = Each QueuedCaptionMsg
        If queue\timeStart > 0.0 Then
            queue\timeStart = queue\timeStart - FPSFactor
        Else
            txtLine$ = queue\txt

            hasSplit% = False

            ; Split long lines of text into multiple lines.
            While Len(txtLine) > 0
                stringRight# = GetCaptionBoxLeft()+5+AAStringWidth(txtLine)
                If stringRight > GetCaptionBoxLeft()+GetCaptionBoxWidth()-5 Then
                    nextLine$ = ""
                    While stringRight > GetCaptionBoxLeft()+GetCaptionBoxWidth()-5
                        nextLine = Right(txtLine,1) + nextLine
                        txtLine = Left(txtLine, Len(txtLine)-1)
                        stringRight = GetCaptionBoxLeft()+5+AAStringWidth(txtLine)
                    Wend

                    oldTxtLine$ = txtLine

                    While Right(txtLine, 1) <> " "
                        nextLine = Right(txtLine,1) + nextLine
                        txtLine = Left(txtLine, Len(txtLine)-1)

                        If txtLine = "" Then
                            txtLine = oldTxtLine
                            Exit
                        EndIf
                    Wend

                    If hasSplit = True Then
                        lastC.CaptionMsg = Last CaptionMsg
                        lastC\yPos = lastC\yPos - CaptionTextHeight
                        lastC\curYPos = lastC\curYPos - CaptionTextHeight
                    EndIf
                    CreateCaptionMsg(queue\soundPath, queue\sound, Trim(txtLine), queue\timeLeft, queue\r, queue\g, queue\b, queue\isItalic, queue\isBold, queue\isGlitch)
                    hasSplit = True
                    txtLine = nextLine
                Else
                    CreateCaptionMsg(queue\soundPath, queue\sound, Trim(txtLine), queue\timeLeft, queue\r, queue\g, queue\b, queue\isItalic, queue\isBold, queue\isGlitch)
                    txtLine = ""
                EndIf
            Wend

            Delete queue
        EndIf
    Next

    For c.CaptionMsg = Each CaptionMsg
        c\timeLeft = c\timeLeft - FPSFactor
        If c\timeLeft < 0 Then
            c\alpha = Max(c\alpha-(FPSFactor/7.0), 0.0)
            If c\alpha <= 0.0 Then
                Delete c
            EndIf
        Else
            c\alpha = Min(1.0, c\alpha+(FPSFactor/7.0))
        EndIf
    Next
End Function

Function CaptionGetINIFileSectionLocation%(section$)
	Local Temp%
    SeekFile(CaptionFile, 0)
	Local f% = CaptionFile ; ReadFile(file)
	
	section = Lower(section)
	
	Local n%=0
	While Not Eof(f)
		Local strtemp$ = ReadLine(f)
		n=n+1
		If Left(strtemp,1) = "[" Then
			strtemp$ = Lower(strtemp)
			Temp = Instr(strtemp, section)
			If Temp>0 Then
				If Mid(strtemp, Temp-1, 1)="[" Or Mid(strtemp, Temp-1, 1)="|" Then
					SeekFile(CaptionFile, 0) ; CloseFile f
					Return n
				EndIf
			EndIf
		EndIf
	Wend
	
	SeekFile(CaptionFile, 0) ; CloseFile f
End Function

Function GetCaptionBoxWidth#()
    Return GraphicWidth * 0.75
End Function

Function GetCaptionBoxLeft#()
    Return (GraphicWidth / 2)+1-(GetCaptionBoxWidth()/2)
End Function

Function GetCaptionBoxTop#()
    Return (GraphicHeight) * 0.8
End Function

Function DrawCaptions()
    lines% = 0
    For c.CaptionMsg = Each CaptionMsg
        lines = lines + 1
    Next

    boxTop# = (GetCaptionBoxTop() + CaptionTextHeight) - CaptionTextHeight * lines
    boxHeight# = CaptionTextHeight * lines

    CaptionCurrentBoxTop = CurveValue(boxTop, CaptionCurrentBoxTop, 7.0)
    CaptionCurrentBoxHeight = CurveValue(boxHeight, CaptionCurrentBoxHeight, 7.0)
    
    ; Draw box
    Color 20,20,20
    Rect GetCaptionBoxLeft(),CaptionCurrentBoxTop,GetCaptionBoxWidth(),CaptionCurrentBoxHeight

    ; Draw text
    lines = -1
    For c.CaptionMsg = Each CaptionMsg
        lines = lines + 1
        txt$ = c\txt

        c\yPos = boxTop+(CaptionTextHeight*lines)+5
        c\curYPos = CurveValue(c\yPos, c\curYPos, 7.0)

        Color c\r,c\g,c\b

        If c\isItalic=false And c\isBold=false Then
            AASetFont Font1
        ElseIf c\isItalic=true And c\IsBold=true Then
            AASetFont Font1BoldItalic
        ElseIf c\isItalic=true Then
            AASetFont Font1Italic
        ElseIf c\isBold=true Then
            AASetFont Font1Bold
        EndIf

        If c\isGlitch Then
            For i = 0 To Rand(30,60)
				txt = Replace(c\txt,Mid(c\txt,Rand(1,Len(txt)-1),1),Chr(Rand(130,250)))
			Next
        EndIf

        AAText(GetCaptionBoxLeft()+5,c\curYPos,txt,False,False,c\alpha)
    Next

End Function

; Parse the caption settings
; Example of caption settings: <r=255,g=255,b=255,color=orange,italic=true,bold=true,glitch=true>
Function ParseCaptionSettings$(c.QueuedCaptionMsg)
    txt$ = c\txt
    startLeft% = 0
    startRight% = 0

    temp% = 0
    While temp < Len(txt)
        temp = temp + 1

        If Mid(txt, temp, 1) = "<"
            startLeft = temp-1
            While Mid(txt, temp, 1) <> ">" And temp < Len(txt)
                If Mid(txt, temp, 1) = "<" Then
                    temp = temp + 1
                EndIf

                key$ = ""
                value$ = ""
                While Mid(txt, temp, 1) <> "=" And Mid(txt, temp, 1) <> ">" And temp < Len(txt)
                    key = key + Mid(txt, temp, 1)
                    temp = temp + 1
                Wend

                If Mid(txt, temp, 1) = "=" Then
                    temp = temp + 1
                EndIf

                While Mid(txt, temp, 1) <> "," And Mid(txt, temp, 1) <> ">" And temp < Len(txt)
                    value = value + Mid(txt, temp, 1)
                    temp = temp + 1
                Wend

                If Mid(txt, temp, 1) = "," Then
                    temp = temp + 1
                EndIf

                If Trim(Lower(key)) = "color" Then
                    For clr.CaptionColor = Each CaptionColor
                        If Trim(Lower(value)) = clr\name Then
                            c\r = clr\r
                            c\g = clr\g
                            c\b = clr\b
                        EndIf
                    Next
                EndIf
                If Trim(Lower(key)) = "r" Then
                    c\r = Int(Trim(Lower(value)))
                EndIf
                If Trim(Lower(key)) = "g" Then
                    c\g = Int(Trim(Lower(value)))
                EndIf
                If Trim(Lower(key)) = "b" Then
                    c\b = Int(Trim(Lower(value)))
                EndIf
                If Trim(Lower(key)) = "bold" Then
                    If Trim(Lower(value)) = "true" Then
                        c\isBold = true
                    ElseIf Trim(Lower(value)) = "false" Then
                        c\isBold = false
                    EndIf
                EndIf
                If Trim(Lower(key)) = "italic" Then
                    If Trim(Lower(value)) = "true" Then
                        c\isItalic = true
                    ElseIf Lower(value) = "false" Then
                        c\isItalic = false
                    EndIf
                EndIf
                If Trim(Lower(key)) = "glitch" Then
                    If Trim(Lower(value)) = "true" Then
                        c\isGlitch = true
                    ElseIf Trim(Lower(value)) = "false" Then
                        c\isGlitch = false
                    EndIf
                EndIf
                If Trim(Lower(key)) = "length" Then
                    c\timeLeft = (Float(Trim(Lower(value))) + 5.0) * 70.0
                EndIf
            Wend
        EndIf

        If Mid(txt, temp, 1) = ">"
            txt = Left(txt, startLeft) + Right(txt, Len(txt)-temp)
            temp = 0
        EndIf
    Wend

    Return txt
End Function

Function CreateCaptionToken(soundPath$, sound.Sound)
    Local TemporaryString$ = ""

    Local start% = CaptionGetINIFileSectionLocation(soundPath)

    SeekFile(CaptionFile, 0)
    Local f% = CaptionFile ; ReadFile(CaptionFile)

    local n%=0
    While Not Eof(f)
        local strtemp$ = ReadLine(f)
        n=n+1
        If n=start Then
            Repeat
                TemporaryString = ReadLine(f)

                If Instr(TemporaryString, "=") <> 0
                    If Trim(Left(TemporaryString, Max(Instr(TemporaryString, "=") - 1, 0))) = "text" Then
                        QueueCaptionMsg(soundPath, sound, Trim( Right(TemporaryString,Len(TemporaryString)-Instr(TemporaryString,"=")) ), 0, 10)
                    Else
                        QueueCaptionMsg(soundPath, sound, Trim( Right(TemporaryString,Len(TemporaryString)-Instr(TemporaryString,"=")) ), Float(Trim(Left(TemporaryString, Max(Instr(TemporaryString, "=") - 1, 0)))), 10)
                    EndIf
                EndIf

            Until Left(TemporaryString, 1) = "[" Or Eof(f)
            SeekFile(CaptionFile, 0 ) ; CloseFile f

            Return
        EndIf
    Wend

    SeekFile(CaptionFile, 0 ) ; CloseFile f
End Function

Function RemoveCaptionToken(sound.Sound)
    For queue.QueuedCaptionMsg = Each QueuedCaptionMsg
        If queue\sound = sound Then
            Delete queue
        EndIf
    Next
End Function

Function ClearCaptions()
    For caption.CaptionMsg = Each CaptionMsg
        Delete caption
    Next
End Function

Function QueueCaptionMsg(soundPath$, sound.Sound, txt$, timeStart#, timeLeft#)
    If txt = "" Or Left(txt,1) = "[" Then
        Return
    EndIf

    Local queue.QueuedCaptionMsg = new QueuedCaptionMsg
    ; Insert c before First CaptionMsg

    queue\soundPath = soundPath
    queue\sound = sound

    queue\txt = txt

    queue\r = 255
    queue\g = 255
    queue\b = 255
    queue\isItalic = false
    queue\isBold = false
    queue\isGlitch = false

    queue\timeLeft = timeLeft * 70

    
    queue\timeStart = timeStart * 70

    queue\txt = ParseCaptionSettings(queue)

    Insert queue Before First QueuedCaptionMsg
End Function

Function CreateCaptionMsg(soundPath$, sound.Sound, txt$, timeLeft#, r%=255, g%=255, b%=255, isItalic%=false, isBold%=false, isGlitch%=false)
    If DeafTimer > 0 Then
        Return
    EndIf

    If sound <> Null Then
        Local isChannelPlaying% = false
        For i = 0 To 31
            If sound\channels[i] <> 0 Then
                If ChannelPlaying(sound\channels[i]) Then
                    isChannelPlaying = true
                EndIf
            EndIf
        Next

        If isChannelPlaying = false Then
            Return
        EndIf
    EndIf

    Local c.CaptionMsg = new CaptionMsg

    c\soundPath = soundPath
    c\sound = sound
    c\txt = txt
    c\r = r
    c\g = g
    c\b = b
    c\isItalic = isItalic
    c\isBold = isBold
    c\isGlitch = isGlitch
    c\timeLeft = timeLeft
    c\alpha = 0

    lines% = 0
    For caption.CaptionMsg = Each CaptionMsg
        lines = lines + 1
    Next

    boxTop# = (GetCaptionBoxTop() + CaptionTextHeight) - CaptionTextHeight * lines
    boxHeight# = CaptionTextHeight * lines

    c\yPos = (boxTop+boxHeight)-CaptionTextHeight+5
    c\curYPos = (boxTop+boxHeight)-CaptionTextHeight+5

    Insert c after Last CaptionMsg
End Function

Function CreateCaptionColor(name$, r%, g%, b%)
    Local c.CaptionColor = new CaptionColor

    c\name = name
    c\r = r
    c\g = g
    c\b = b
End Function

CreateCaptionColor("red", 255, 0, 0)
CreateCaptionColor("orange", 255, 128, 0)
CreateCaptionColor("yellow", 255, 255, 0)
CreateCaptionColor("lime", 128, 255, 0)
CreateCaptionColor("green", 0, 255, 0)
CreateCaptionColor("springgreen", 0, 255, 128)
CreateCaptionColor("cyan", 0, 255, 255)
CreateCaptionColor("lightblue", 0, 128, 255)
CreateCaptionColor("blue", 0, 0, 255)
CreateCaptionColor("purple", 128, 0, 255)
CreateCaptionColor("magenta", 255, 0, 255)
CreateCaptionColor("pink", 255, 0, 128)
CreateCaptionColor("gray", 128, 128, 128)
CreateCaptionColor("grey", 128, 128, 128)
CreateCaptionColor("black", 0, 0, 0)
CreateCaptionColor("white", 255, 255, 255)

CaptionFile = ReadFile(CaptionFilePath)