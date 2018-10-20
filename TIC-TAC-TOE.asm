TITLE TIC-TAC-TOE ;Titlos ergou

DEDOMENA SEGMENT

    PLAYER_MSG_BUFFER DB "P","L","A","Y","E","R"," "," "    ; H etiketa mnimis pou emfanizete panw aristera stin othoni tou paixnidiou 
    TURNmsg DB "G","i","v","e"," ","x"," ","a","n","d"," ","y"," ",":"," "," "   ; H etiketa mnimis pou leei ston paikti na dwsei x 
                                                                                 ;kai y
    WINmsg DB "PLAYER   WON !!!"   ; H etiketa mnimis pou leei poios paiktis nikise (sindiazete me tin etiketa mnimis "PLAYER")
    TEMP DB 0   ; H etiketa mnimis, h opoia xrisimopoieitai gia na apothikeusoume tin epilogi tou paikti (stin periptwsi pou paizei 
                ;monos tou me ton upologisti) sxetika me to an thelei na paiksei 1os h 2os
    HOW_MANY DB 0 ; H etiketa mnimis, h opoia xrisimopoieitai gia na apothikeusoume tin epilogi tou paikti, sxetika me to an thelei na 
                  ;paiksei monos tou (opote o allos paiktis einai o upologistis) h me kapoio fisiko proswpo
    PLAYER DB 1 ;Se auti thn etiketa mnimis apothikeuetai o paiktis (ws timi 1 gia ton 1o kai 2 gia ton 2o) pou prokeitai na paiksei 
                ;amesws meta 
    BUFFER DB 9 dup(0) ; O pinakas autos einai o pinakas twn "X", diladi gia kathe "X" pou topothetitai panw sto tamplo tis trilizas o 
                       ;pinakas pairnei tin timi 1 se sigkekrimeni thesi tou pinaka kata antistoixia me to tetragwno tis trilizas pou 
                       ;topothetithike (kanodas arithmisi apo to 0)
    BUFFER2 DB 9 dup(0) ; O pinakas autos einai o pinakas twn "O", diladi gia kathe "O" pou topothetitai panw sto tamplo tis trilizas 
                        ;o pinakas pairnei tin timi 1 se sigkekrimeni thesi tou pinaka kata antistoixia me to tetragwno tis trilizas 
                        ;pou topothetithike (kanodas arithmisi apo to 0)
    DRAW DB 0  ;Metraei tis sunolikes kiniseis kai twn 2 paiktwn
    DRAWmsg DB "DRAW!" ; H etiketa mnimis me to minima pou emfanizetai stin periptwsi tis ispalias
    COMMAND DB 0  ; Auti i etiketa mnimis adistoixei sti sidetagmeni x kai xrisimopoieitai kirios apo ton upologisti otan einai autos o 
                  ;allos paiktis kai genika gia tin teleutaia kinisi pou apomenei sto paixnidi 
    COMMAND2 DB 0 ; Auti i etiketa mnimis adistoixei sti sidetagmeni y kai xrisimopoieitai kirios apo ton upologisti otan einai autos o 
                  ;allos paiktis kai genika gia tin teleutaia kinisi pou apomenei sto paixnidi  
    TIMES2 DB 0 ; Auti i etiketa mnimis simvalei stin 1h kinisi tou upologisti (stin periptwsi pou paizei me ton upologisti)
    AGAINMSG DB "PLAY AGAIN?" ; H etiketa mnimis pou emfanizetai sto telos tou paixnidiou kai rwtaei ton xristi an thelei na xanapaixei
    FORES DB 0 ; Apothikeuetai i apadisi pou dinei o xristis sto telos tou paixnidiou sxetika me to an thelei na xanapaixei ("Y" gia nai 
               ;kai "O" gia oxi) 
    ENTERMSG DB "PRESS ENTER TO CONTINUE" ; To minima autis tis etiketas pliroforei ton xristi oti gia na sinexisei xriazetai na patisei to 
                                          ;"ENTER"
    BUFF DB 6 dup(0) ;O pinakas autos exei skopo na kratisi to score twn 2 paiktwn ,diladi poses nikes exei o kathenas (Oi 1es theseis tou 
                     ;pinaka (0-2) einai gia ton 1o paikti kai apo 3-5 gia ton 2o),o megaliteros arithmos pou borei na anaparastisei einai 
                     ;to 999 gia ton kathe paikti ksexwrista
    CLEARBMSG DB "                         " ; To sunolo twn kenwn xaraktirwn autis tis etiketas mas boithaei na eksafanisoume minimata pou 
                                             ;prepei na eksafanistoun se kapoies periptwseis
         
    ;Oi parakatw etiketes mnimis mexri tin "MESSAGE7" periexoun diafora minimata pou emfanizodai sto paixnidi
         
    EPILOGI1 DB "1"," ","P","L","A","Y","E","R"," "," "," "," ","2"," ", "P","L","A","Y","E","R","S" ;Emfanizetai sti synartisi "PLAYER33"
    EPILOGI2 DB "F","I","R","S","T"," "," "," ","S","E","C","O","N","D"   ;Emfanizetai sti synartisi "BOX"
    MESSAGE1 db "P","R","E","S","S"," ","K","E","Y"," ","1"," ","O","R"," ","2"  ;Emfanizetai sti synartisi "KEY"
    MESSAGE2 db "C","H","O","O","S","E"," ","Y","O","U","R"," ","T","U","R","N",".",".","."  ;Emfanizetai sti synartisi "TURN"
    MESSAGE3 db "T","Y","P","E"," ","Y","O","U","R"," ","N","A","M","E",".",".","."    ;Emfanizetai sti synartisi "NAME_MSG"
    MESSAGE4 db "P","L","A","Y","E","R"," ","1",":"," "   ;Emfanizetai sti synartisi "PLAYER1_MSG"
    MESSAGE5 db "P","L","A","Y","E","R"," ","2",":"," "   ;Emfanizetai sti synartisi "PLAYER2_MSG"
    MESSAGE6 db "P","R","E","S","S"," ","A","N","Y"," ","K","E","Y"," ","T","O"," ","C","O","N","T","I","N","U","E"  ;Emfanizetai sti 
                                                                                                                     ;synartisi "KEYGAN"
    WINSMSG db "NAME                 WINS" ;Emfanizetai sti synartisi "NAMES"
    MESSAGE7 db "P","R","E","S","S"," ","K","E","Y"," ","Y"," ","O","R"," ","N" ;Emfanizetai sti synartisi "KEY2"
    MESSAGE8 db "Invalid Value! Try Again!" ;Emfanizetai sti synartisi "INVALID_MESSAGE"
    ;Oi parakatw etiketes aforoun ti dimiourgia tou arxeiou keimenou txt
         
    dir DB "c:\RESULTS",0    ;Fakelos me onoma "RESULTS"
    file DB "c:\RESULTS\RESULTS.txt",0  ;Arxeio keimenou me onoma "RESULTS" 
         
    handle dw ?  ;Deiktis gia na exoume prosbasi sto arxeio
         
    ;Ta parakatw minimata grafoun sto arxeio ta apotelesmata tou paixnidiou me to text3 na periexei tis nikes tou 1ou kai to text5 tis nikes 
    ;tou 2ou paikti
         
     text DB 10,13,10,13,"                               Hall Of Fame",13,10,13,10,13,10
     text_size = $ - offset text
         
     text1 DB "   Name                                                                Wins",13,10,13,10
     text_size1 = $ - offset text1
         
     text2 DB "   ...................................................................."
     text_size2 = $ - offset text2
         
     text3 DB "..0",13,10,13,10
     text_size3 = $ - offset text3
         
     text4 DB "   ...................................................................."
     text_size4 = $ - offset text4                                                 
         
     text5 DB "..0",13,10,13,10
     text_size5 = $ - offset text5

DEDOMENA ENDS

KODIKAS SEGMENT
ARXH: MOV AX,DEDOMENA
      MOV DS,AX
        
        CALL graph    ;Kalitai gia na energopoiithei i katastasi grafikwn
        CALL screen1  ;Periexei to menou apo to opoio o xristis kaleitai na epileksei plithos paiktwn
        CALL TAKE_COMMAND_1  ;Pernei tin epilogi pou dwthike stin "screen1"
        CMP HOW_MANY,2  ; Autos o elegxos ginetai gia na katalavei to programma an prepei na dwsei ston xristi tin dinatotita na epileksei
                        ;an thelei na paiksei 1os h 2os stin periptwsi pou epelekse na paiksei monos tou,diaforetika ta 2 fisika proswpa
                        ; boroun na simfwnisoun monoi tous poios tha paiksei 1os h 2os
        JE NEXTSTEP
        ;Edw ektelountai oi edoles gia ton 1 paikti 
        CALL graph ;Kalitai gia na energopoiithei i katastasi grafikwn-Nea othoni
        CALL screen2 ;Zhtaei apo ton xristi na epileksei an thelei na paiksei 1os h 2os
        CALL TAKE_COMMAND_1B  ;Pairnei tin epilogi tou apo panw erwtimatos tis "screen2"
        CALL graph   ;Kalitai gia na energopoiithei i katastasi grafikwn-Nea othoni
        CALL SCREEN3 ;Edw oi xristes-paiktes kaloudai na dwsoun ta onomata tous
        JMP SYNEXIA  ;Afou oi paiktes einai enas metavainoume sto kirios paixnidi
        
        NEXTSTEP:    ;Edw ekteloudai oi edoles gia tous 2 paiktes
        CALL graph   ;Kalitai gia na energopoiithei i katastasi grafikwn
        CALL SCREEN3 ;Edw oi xristes-paiktes kaloudai na dwsoun ta onomata tous
        
        SYNEXIA:
        
        CALL graph   ;Kalitai gia na energopoiithei i katastasi grafikwn-Nea othoni
        CALL HALL_OF_FAME   ;Emfanisi twn onomatwn twn paiktwn kai twn nikwn tous
        CALL TABLE          ;Dimiourgia tis trilizas se grafika
        CALL USER_MESSAGES  ;Ta minimata pou emfanizodai stous paiktes gia na dwsoun x kai y , kathws kai na enimerwsoun poia nou seira
                            ;einai
        
        THE_LOOP:     ;O vroxos epanalipsis tou paixnidiou-Borei na ginei kai atermwn
        
        CALL TURNINGPLAYER  ;Edw i etiketa mnimis "PLAYER" ektiponei ton arithmo 1 h 2 analoga poia nou seira einai na paiksei 
        
        CALL TAKE_COMMAND_2  ;Edw oi xristes analoga me ti seira pou exoun kaloudai na dwsoun x kai y gia tin kinisei tous
        CALL CHECK4WINNER    ;Elegxos ean vrethike nikitis
        
        CMP DI,1    ;Ean kapoios nikisei to DI pairnei tin timi 1 kai pigenei sto telos tou paixnidiou
        JE THISEND  ;Metavasi sto telos tou paixnidiou
        
        CALL DRAW_CHECK   ;Ean den exei vrethei nikitis mexri tin teleutaia kinisi tote to DI pairnei tin timi 1 
        
        CMP DI,1    ;Ean den uparxei nikitis apo ton panw elegxo tote to DI prepei na einai 1 
        JE THISEND  ;Metavasi sto telos tou paixnidiou
        
        ;Analoga me to poios paiktis epaikse teleutaios i etiketa mnimis "PLAYER" pairnei timi pou na adistoixei ston epomeno
        ;paikti
        
        CMP PLAYER,1  ;Otan epaikse o 1os paiktis ginetai 2
        JE INCPLAYER  ;Metavasi stin allagi se 2
        
        DEC PLAYER    ;Otan epaikse o 2os paiktis ginetai 1
        JMP THE_LOOP  ;Pigenei stin arxi tou vroxou
        
        INCPLAYER:    ;Allagi se 2
        INC PLAYER
        
        JMP THE_LOOP  ;Pigenei stin arxi tou vroxou
        
        THISEND:
        
        CALL ARCHIVE  ;Dimiourgia tou arxeiou keimenou me ta apotelesmata tou paixnidiou
        
        CALL RESET    ;Ektos apo ta onomata kai tis nikes twn paiktwn oles oi alles etiketes pou metavalodai sto paixnidi epanaferoun tis
                      ;arxikes times tous
        CMP FORES,59H ;An o xristis epileksei na ksanapeksei tote to paixnidi xekinaei pali exodas tis nikes kai ta onomata opos einai
        JE SYNEXIA
        
      MOV AH,4CH
      INT 21h

;Synarthseis
;---------------------------------------- 

TABLE PROC  ;Dimiourgia tou tablou tis trilizas
    
    CALL graph
    
    MOV CX,95 ;arxikopoihsh grammh
    MOV DX,70 ;arxikopoihsh sthlhs
    
    CALL draw_horizontal   ;Ginetai eggrafi pixel orizodia
    
    MOV CX,95 ;arxikopoihsh grammh
    MOV DX,100 ;arxikopoihsh sthlhs
     
    CALL draw_horizontal   ;Ginetai eggrafi pixel orizodia
    
    MOV CX,135 ;arxikopoihsh grammh
    MOV DX,40 ;arxikopoihsh sthlhs
    
    CALL draw_vertical     ;Ginetai eggrafi pixel katheta
    
    MOV CX,175 ;arxikopoihsh grammh
    MOV DX,40 ;arxikopoihsh sthlhs
    
    CALL draw_vertical     ;Ginetai eggrafi pixel katheta
    
    RET
    
TABLE ENDP

;---------------------------------------- 

N_ADA PROC    ;Arithmisi twn nikwn tou 1ou paikti  
    
    PUSH DX
    PUSH SI
    PUSH CX
    PUSH AX
    
    MOV DL,10
    
    ;Ston BUFF ginetai i kratisi mexri kai 3psifiou arithmou gia kathe paikti
    ;Kathe fora pou nikaei ginetai auksisi kata 1 sto periexomeno tis teleutaias thesis pou dikaioutai o 1os paiktis tou pinaka diladi sti 2
    ;Ginodai sigkriseis me to 10. Otan to periexomeno kapoias thesis tou pinaka ginei 10 tote automatos midenizetai kai auxanei i amesws
    ;mikroteri kathws i metrisi ksekinaei apo tin teleutaia thesi to 2 edw pou adistoixei stin monada, to 1 stin dekada kai to 0 stin 100ada
    ;Sto text3 apothikeuetai afou metatrapei se xaraktira to kathe psifio tou arithmou pou proekipse xwris midenika stin arxi (p.x. 010=10)
    
    CMP BUFF[2],DL
    JNB EPI10
    INC BUFF[2]
    
    JMP N_END
    
    EPI10:
    
    MOV BUFF[2],0
    INC BUFF[1]
    
    CMP BUFF[1],DL
    JNB EPI100
    
    JMP N_END
    
    EPI100:
    
    MOV BUFF[1],0
    INC BUFF[0]
    
    N_END:
    
    CMP BUFF[0],0
    JE CMP2
    MOV CX,3
    XOR SI,SI
    
    N_ADA0:
    
    MOV AL,BUFF[SI]
    ADD AL,30H
    MOV TEXT3[SI],AL
    INC SI
    LOOP N_ADA0
    
    JMP N_ENDA
    
    CMP2:
    
    CMP BUFF[1],0
    JE CMP3
    MOV CX,2
    MOV SI,1
    
    N_ADA00:
    
    MOV AL,BUFF[SI]
    ADD AL,30H
    MOV TEXT3[SI],AL
    INC SI
    LOOP N_ADA00
    
    JMP N_ENDA
    
    CMP3:
    
    MOV AL,BUFF[2]
    ADD AL,30H
    MOV TEXT3[2],AL
    
    N_ENDA:
    
    ;Epanafora arxikwn timwn twn kataxwritwn
    
    POP AX
    POP CX
    POP SI
    POP DX
    
    RET
    
N_ADA ENDP

;----------------------------------------
N_ADA2 PROC  ;Arithmisi twn nikwn tou 2ou paikti
    
    PUSH DX
    PUSH SI
    PUSH CX
    PUSH AX
    
    MOV DL,10
    
    ;Ston BUFF ginetai i kratisi mexri kai 3psifiou arithmou gia kathe paikti
    ;Kathe fora pou nikaei ginetai auksisi kata 1 sto periexomeno tis teleutaias thesis pou dikaioutai o 1os paiktis tou pinaka diladi sti 5
    ;Ginodai sigkriseis me to 10. Otan to periexomeno kapoias thesis tou pinaka ginei 10 tote automatos midenizetai kai auxanei i amesws
    ;mikroteri kathws i metrisi ksekinaei apo tin teleutaia thesi to 5 edw pou adistoixei stin monada, to 4 stin dekada kai to 3 stin 100ada
    ;Sto text3 apothikeuetai afou metatrapei se xaraktira to kathe psifio tou arithmou pou proekipse xwris midenika stin arxi (p.x. 001=1)
    
    CMP BUFF[5],DL
    JNB EPI10B
    INC BUFF[5]
    
    JMP N_ENDB
    
    EPI10B:
    
    MOV BUFF[5],0
    INC BUFF[4]
    
    CMP BUFF[4],DL
    JNB EPI100B
    
    JMP N_ENDB
    
    EPI100B:
    
    MOV BUFF[4],0
    INC BUFF[3]
    
    N_ENDB:
    
    CMP BUFF[3],0
    JE CMP4
    MOV CX,3
    MOV SI,3
    N_ADA01:
    MOV AL,BUFF[SI]
    ADD AL,30H
    MOV TEXT5[SI-3],AL
    INC SI
    LOOP N_ADA01
    
    JMP N_ENDAB
    
    CMP4:
    
    CMP BUFF[1],0
    JE CMP5
    MOV CX,2
    MOV SI,4
    N_ADA001:
    MOV AL,BUFF[SI]
    ADD AL,30H
    MOV TEXT5[SI-3],AL
    INC SI
    LOOP N_ADA001
    
    JMP N_ENDAB
    
    CMP5:
    
    MOV AL,BUFF[5]
    ADD AL,30H
    MOV TEXT5[2],AL
    
    N_ENDAB:
    
    ;Epanafora arxikwn timwn twn kataxwritwn
    
    POP AX
    POP CX
    POP SI
    POP DX
    
    RET
    
N_ADA2 ENDP
;----------------------------------------

ARCHIVE PROC      ;Oles oi edoles pou apaitoudai gia tin dimiourgia tou arxeiou
    
    ;Oi parakatw sigkriseis ginodai gia na dwthei o podos ston nikiti i kai oxi anexoume isopalia
    
    CMP PLAYER,1
    JNE GIVEPOINT
    CMP PLAYER,3
    JE START
    CALL N_ADA
    JMP START
    GIVEPOINT:
    CMP PLAYER,3
    JE START
    CALL N_ADA2
        
    START:
        
    MOV AX, CS
    MOV DX, AX
    MOV ES, AX
        
    MOV DX, offset dir
    MOV AH, 39h
    INT 21h
        
    MOV AH, 3ch
    MOV CX, 0
    MOV DX, offset file
    INT 21h
    JC ERR
    MOV handle, AX
        
    MOV DX, offset text
    MOV CX, text_size
    CALL ARCH_WRITE
        
    MOV DX, offset text1
    MOV CX, text_size1
    CALL ARCH_WRITE
        
    MOV DX, offset text2
    MOV CX, text_size2
    CALL ARCH_WRITE

    MOV DX, offset text3
    MOV CX, text_size3
    CALL ARCH_WRITE
        
    MOV DX, offset text4
    MOV CX, text_size4
    CALL ARCH_WRITE
        
    MOV DX, offset text5
    MOV CX, text_size5
    CALL ARCH_WRITE
        
    MOV AH, 3eh
    MOV BX, handle
    INT 21h
    ERR:
    NOP
    
    RET
    
ARCHIVE ENDP
;----------------------------------------
ARCH_WRITE PROC     ;H diakopi eggrafis sto arxeio ws sinartisi
    
    MOV AH, 40h
    MOV BX, handle
    INT 21h
    
    RET
    
ARCH_WRITE ENDP    
;----------------------------------------

CHECK4WINNER PROC         ;Elegxos gia nikiti
    
    PUSH DX
    PUSH AX
    PUSH CX
    PUSH SI
    PUSH BX
    
    ;Logiki twn elegxwn: Yparxei emfoleumenos vroxos se kathe vroxo me to PUSH/POP CX kratame tin timi tou exoterikou vroxou. To SI einai 
    ;deiktis pou dixnei an stin x thesei uparxei "O" h "X" an uparxei auxanei o metritis DI , an ginei 3 stamatanai i epomenoi elegxoi kai 
    ;pigenei stin anadiksi tou nikiti.To BX mas voithaei stin metatopisi tou dikti SI. 
    
    ;Elegxos twn "X" orizodia
    
    MOV CX,3
    XOR SI,SI
    XOR BX,BX
    CHECK4X_HORISONTAL:
    PUSH CX
    MOV CX,3
    CHECK4X_HORISONTAL_1:
    CMP BUFFER[SI],1
    JNE NEXTLINE
    INC SI
    LOOP CHECK4X_HORISONTAL_1
    POP CX
    JMP GIVEWINNER
    NEXTLINE:
    XOR SI,SI
    ADD BX,3
    ADD SI,BX
    POP CX
    LOOP CHECK4X_HORISONTAL
    
    ;.....................................
    
    ;Elegxos twn "O" orizodia
    
    MOV CX,3
    XOR SI,SI
    XOR BX,BX
    CHECK4O_HORISONTAL:
    PUSH CX
    MOV CX,3
    CHECK4O_HORISONTAL_1:
    CMP BUFFER2[SI],1
    JNE NEXTLINE2
    INC SI
    LOOP CHECK4O_HORISONTAL_1
    POP CX
    JMP GIVEWINNER
    NEXTLINE2:
    XOR SI,SI
    ADD BX,3
    ADD SI,BX
    POP CX
    LOOP CHECK4O_HORISONTAL
    
    ;.....................................
    
    ;Elegxos twn "X" katheta
    
    MOV CX,3
    XOR SI,SI
    MOV BX,1
    CHECK4X_VERTICAL:
    PUSH CX
    MOV CX,3
    CHECK4X_VERTICAL_1:
    CMP BUFFER[SI],1
    JNE NEXTLINE3
    ADD SI,3
    LOOP CHECK4X_VERTICAL_1
    POP CX
    JMP GIVEWINNER
    NEXTLINE3:
    MOV SI,BX
    INC BX 
    POP CX
    LOOP CHECK4X_VERTICAL
    
    ;.....................................
    
    ;Elegxos twn "O" katheta
    
    MOV CX,3
    XOR SI,SI
    MOV BX,1
    CHECK4O_VERTICAL:
    PUSH CX
    MOV CX,3
    XOR SI,SI
    CHECK4O_VERTICAL_1:
    CMP BUFFER2[SI],1
    JNE NEXTLINE4
    ADD SI,3
    LOOP CHECK4O_VERTICAL_1
    POP CX
    JMP GIVEWINNER
    NEXTLINE4:
    MOV SI,BX
    INC BX
    POP CX
    LOOP CHECK4O_VERTICAL
    
    ;.....................................
    
    ;Elegxos twn "X" kai stis 2 diagwnious
    
    MOV CX,2
    XOR SI,SI
    MOV BX,4
    
    CHECK4X_DIAGONAL: 
    PUSH CX
    MOV CX,3
    CHECK4X_DIAGONAL_1:
    CMP BUFFER[SI],1
    JNE NEXTLINE5
    ADD SI,BX
    LOOP CHECK4X_DIAGONAL_1
    POP CX
    JMP GIVEWINNER
    NEXTLINE5:
    MOV BX,2
    MOV SI,BX
    POP CX
    LOOP CHECK4X_DIAGONAL
    
    ;.....................................
    
    ;Elegxos twn "O" kai stis 2 diagwnious
    
    MOV CX,2
    XOR SI,SI
    MOV BX,4
    
    CHECK4O_DIAGONAL:
    PUSH CX
    MOV CX,3
    CHECK4O_DIAGONAL_1:
    CMP BUFFER2[SI],1
    JNE NEXTLINE6
    ADD SI,BX
    LOOP CHECK4O_DIAGONAL_1
    POP CX
    JMP GIVEWINNER
    NEXTLINE6:
    MOV BX,2
    MOV SI,BX
    POP CX
    LOOP CHECK4O_DIAGONAL
    
    JMP NO_WINNER   ;Metavasi sto telos an de vrethei nikitis
    
    ;.....................................
    
    GIVEWINNER:
    
    ;.....................................
    
    CALL GRAPH
    
    ;.....................................
    
    ;Emfanisi TIC-TAC-TOE
    
    CALL TAF 
    CALL IOT 
    CALL EPSILON
    CALL ALPHA1
    CALL OMICRON
    CALL CI
    CALL PAULA
    
    MOV DI,1 
    
    ;Emfanisi minimatwn gia to poios einai o nikitis, an theleis na ksanapaikseis
    
    ;.....................................
    
    MOV DL,14
    MOV DH,16
    CALL CURSOR_POSITION
    
    MOV CX,11
    XOR SI,SI
    
    AGAIN_MSG:    ;Emfanisi minimatos
    MOV AL,AGAINMSG[SI]
    MOV BL,8H
    CALL CHAR_OUTPUT
    INC SI
    INC DL
    CALL CURSOR_POSITION
    LOOP AGAIN_MSG 
    
    ;.....................................
    
    MOV DL,12
    MOV DH,12
    CALL CURSOR_POSITION
    
    XOR SI,SI
    MOV CX,16
    
    PLAYERWONMSG: ;Emfanisi minimatos
    MOV AL,WINMSG[SI]
    MOV BL,5H
    CALL CHAR_OUTPUT
    INC SI
    INC DL
    CALL CURSOR_POSITION
    LOOP PLAYERWONMSG
    
    ;.....................................
    
    MOV DL,19
    MOV DH,12
    CALL CURSOR_POSITION
    
    MOV AL,PLAYER   ;Emfanisi arithmou paikti 1 gia ton 1o kai 2 gia ton 2o
    ADD AL,30H
    MOV BL,2
    CALL CHAR_OUTPUT
    
    ;.....................................
    
    CALL KEY2
    
    ;.....................................
    
    GIVEANS1:       ;Apadisi gia to an theleis na ksanapaikseis i oxi
    MOV AH,8H
    INT 21H
    CMP AL,59H
    JE GO1
    CMP AL,4EH
    JNE GIVEANS1
    GO1:
    MOV FORES,AL

    NO_WINNER:
    
    ;Epanafora arxikwn timwn twn kataxwritwn
    
    POP BX
    POP SI
    POP CX
    POP AX
    POP DX
    
    RET
    
CHECK4WINNER ENDP    

;----------------------------------------

USER_MESSAGES PROC   ;Minimata paixnidiou "PLAYER / Give x and y:"

        PUSH DX
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DI
        
        ;Sidetaumenes gia ti thesi tou kersora
        
        MOV DL,1    
        MOV DH,1
        CALL CURSOR_POSITION  ;Sinartisi pou thetei ton kersora
        INC DL 
        
        ;............................
        
        XOR DI,DI
        MOV CX,7
        
        PLAYER_MSG:
        
        MOV AL,PLAYER_MSG_BUFFER[DI]
        CALL CURSOR_POSITION
        INC DL
        MOV BL,04H
        CALL CHAR_OUTPUT
        INC DI
        LOOP PLAYER_MSG
         
        ;............................
        
        MOV DL,20
        MOV DH,1
        CALL CURSOR_POSITION
        INC DL
        
        ;............................
        
        XOR DI,DI
        MOV CX,15
        
        TURN_MSG:
        
        MOV AL,TURNmsg[DI]
        CALL CURSOR_POSITION
        INC DL
        MOV BL,06H        ;Xroma xaraktira
        CALL CHAR_OUTPUT  ;Emfanisi xaraktira
        INC DI
        LOOP TURN_MSG
        
        POP DI
        POP CX
        POP BX
        POP AX
        POP DX

        RET

USER_MESSAGES ENDP
;-----------------------------------------
TURNINGPLAYER PROC      ;Emfanisi arithmou paikti 1 h 2
        
        PUSH DX
        PUSH BX
        PUSH AX
        
        MOV DL,9
        MOV DH,1
        CALL CURSOR_POSITION
        INC DL
        MOV AL,PLAYER
        ADD AL,30H
        MOV BL,03H
        CALL CHAR_OUTPUT
        
        POP AX
        POP BX
        POP DX
    
        RET
    
TURNINGPLAYER ENDP

;-----------------------------------------    
X_DRAW_2 PROC      ;Zwgrafisma tou X tis 2hs diagwniou
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH DI
        
        MOV DI,0 ;metrhths
        MOV BH,0 ;selida emfanishs
        
        sloop_xx:
        MOV AH,0CH ;emfanish pixel
        
        MOV AL,5H ;xrwma pixel mple
        
        INT 10H
        
        INC DI
        INC CX
        DEC DX ;aukshsh ths grammhs
        CMP DI,18 ;mhkos grammhs
        JNE sloop_xx
        
        POP DI
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET
        
X_DRAW_2 ENDP
;-----------------------------------------
X_DRAW_1 PROC     ;Zwgrafisma tou X tis 1hs diagwniou
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH DI
        
        MOV DI,0 ;metrhths
        MOV BH,0 ;selida emfanishs
        
        sloop_x:
        MOV AH,0CH ;emfanish pixel
        
        MOV AL,5H ;xrwma pixel mple
        
        INT 10H
        
        INC DI
        INC CX
        INC DX ;aukshsh ths grammhs
        CMP DI,18 ;mhkos grammhs
        JNE sloop_x
        
        POP DI
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET
        
X_DRAW_1 ENDP
;-----------------------------------------
TAKE_COMMAND_1 PROC   ;Epilogi plithos paiktwn

        PUSH AX
        PUSH DX
        
        again:        ;Elegxos egkurotitas
        MOV AH,08
        INT 21H
        
        CMP AL,31H
        JE end3
        
        CMP AL,32H
        JE end3
        
        JMP again
        
        end3:
        
        MOV HOW_MANY,AL
        SUB HOW_MANY,30H
        
        
        POP DX
        POP AX
        
        RET

TAKE_COMMAND_1 ENDP
;-----------------------------------------
TAKE_COMMAND_1B PROC   ;Epilogi seiras tou paikti 1os h 2os

        PUSH AX
        PUSH DX
        
        AGAINB:      ;Elegxos egkurotitas
        MOV AH,08
        INT 21H
        
        CMP AL,31H
        JE END3B
        
        CMP AL,32H
        JE END3B
        
        JMP AGAINB
        
        END3B:
        
        MOV TEMP,AL
        SUB TEMP,30H
        
        POP DX
        POP AX
        
        RET

TAKE_COMMAND_1B ENDP
;-----------------------------------------
TAKE_COMMAND_2 PROC  ;Edw ginontai oi elegxoi gia to poios paiktis paizei kai kaloudai oi sinartiseis gia tin eisagwgh tou "X" h tou "O"

        PUSH AX
        PUSH DX
        PUSH CX
        
        CMP PLAYER,1
        JE CALL_INPUT_X
        
        ;.............................
        
        CALL INPUT_O
        JMP TELOS_COM
        
        ;.............................
        
        CALL_INPUT_X:
        CALL INPUT_X
        
        ;.............................
        
        TELOS_COM:
        
        MOV DL,35
        MOV DH,1
        
        MOV CX,2
        
        CLEARIT:    ;Katharizodai minimata kai sidetagmenes
        
        CALL CURSOR_POSITION
        MOV AL," "
        CALL CHAR_OUTPUT
        INC DL
        
        LOOP CLEARIT
        
        CALL CLEARING_MESSAGE
        
        POP CX
        POP DX
        POP AX
        
        RET

TAKE_COMMAND_2 ENDP
;-----------------------------------------
graph PROC    ;Sinartisi grafikwn

        PUSH AX
        
        MOV AL,13H ;leitourgia othonhs grafikwn
        MOV AH,00
        INT 10H
        
        POP AX
        
        RET

graph ENDP
;-----------------------------------------
draw_horizontal PROC       ;Sxediasmos orizodiwn grammwn

        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH DI
        
        MOV DI,0 ;metrhths
        MOV BH,0 ;selida emfanishs
        
        sloop_h:
        MOV AH,0CH ;emfanish pixel
        
        MOV AL,9 ;xrwma pixel mple
        
        INT 10H
        
        INC DI
        INC CX ;aukshsh ths grammhs
        CMP DI,120 ;mhkos grammhs
        JNE sloop_h
        
        POP DI
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET

draw_horizontal ENDP
;-----------------------------------------
draw_vertical PROC    ;Sxediasmos kathetwn grammwn

        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH DI
        
        MOV DI,0 ;metrhths
        MOV BH,0 ;selida emfanishs
        
        sloop_v:
        MOV AH,0CH ;emfanish pixel
        
        MOV AL,2 ;xrwma pixel prasino
        
        INT 10H
        
        INC DI
        INC DX ;aukshsh ths sthlhs
        CMP DI,88 ;mhkos sthlhs
        JNE sloop_v
        
        POP DI
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET

draw_vertical ENDP
;-----------------------------------------
ENTER_MESSAGE PROC     ;Minima "PRESS ENTER TO CONTINUE"
    
    PUSH DX
    PUSH CX
    PUSH AX
    PUSH BX
    
    MOV DL,9
    MOV DH,22
    CALL CURSOR_POSITION
    INC DL
         
    ;....................................
         
    MOV CX,23
    XOR SI,SI
    ENTERB:      ;Vroxos gia tin emfanisi tou minimatos 
    MOV AL,ENTERMSG[SI]
    MOV BL,4H
    CALL CHAR_OUTPUT
    CALL CURSOR_POSITION
    INC DL
    INC SI
    LOOP ENTERB
    
    POP BX
    POP AX
    POP CX
    POP DX
    RET
    
ENTER_MESSAGE ENDP
;-----------------------------------------
INVALID_VALUE PROC      ;Minama gia times sidetagmenwn pou den einai apodektes 
    
    PUSH DX
    PUSH CX
    PUSH AX
    PUSH BX
    
    MOV DL,9
    MOV DH,22
    CALL CURSOR_POSITION
    INC DL
         
    ;....................................
         
    MOV CX,25
    XOR SI,SI
    INVALID:
    MOV AL,MESSAGE8[SI]
    MOV BL,4H
    CALL CHAR_OUTPUT
    CALL CURSOR_POSITION
    INC DL
    INC SI
    LOOP INVALID
    
    POP BX
    POP AX
    POP CX
    POP DX
    RET
    
INVALID_VALUE ENDP
;-----------------------------------------
CLEARING_MESSAGE PROC       ;Minima ekathariseis allwn minimatwn(stin ousua einai ena sunolo apo sinexomenous kenous xaraktires)
    
    PUSH DX
    PUSH CX
    PUSH AX
    PUSH BX
    
    MOV DL,9
    MOV DH,22
    CALL CURSOR_POSITION
    INC DL
         
    ;....................................
         
    MOV CX,25
    XOR SI,SI
    CLEARB:    ;Vroxos
    MOV AL,CLEARBMSG[SI]
    MOV BL,4H
    CALL CHAR_OUTPUT
    CALL CURSOR_POSITION
    INC DL
    INC SI
    LOOP CLEARB
    
    POP BX
    POP AX
    POP CX
    POP DX
    RET
    
CLEARING_MESSAGE ENDP
;-----------------------------------------

INPUT_X PROC  ;Eisagwgi sidetagmenwn gia to "X"
        
        PUSH AX
        PUSH DX
        PUSH CX
        
        ;H logiki einai oti afou o xristis dwsei to x kai ginoun elegxei egkirotitas kai paizei ws fisiko proswpo kai oxi o upologistis tote
        ;kaleitai na dwsei kai to y pali pairnodas apo elegxous ekgurotitas.H teleutaia kinisi tou paixnidiou einai automatopoiimeni
        ;Ean i timi pou dwthike exei ksanadwthei tote prepei na ksanadwsei apo tin arxei x kai y
        
        LOOP_INPUT_X:
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        CMP HOW_MANY,2
        JE NEXT_STEP
        
        CMP TEMP,1
        JE NEXT_STEP 
        
        CALL RANDOM_COMMAND
        ADD COMMAND,30H
        ADD COMMAND2,30H
        MOV AL,COMMAND
        JMP NEXT_STEP_1
         
        NEXT_STEP:
        CMP DRAW,8
        JE AUTOMOVE
        
        MOV AH,08
        INT 21H
        
        CALL CLEARING_MESSAGE
        
        CMP AL,30H
        JB LOOP_INPUT_X
        CMP AL,32H
        JA LOOP_INPUT_X
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV BL,0CH
        CALL CHAR_OUTPUT
        
        JMP NEXT_STEP_1
        ;....................................
        AUTOMOVE:
        
        CALL RANDOM_COMMAND
        ADD COMMAND,30H
        ADD COMMAND2,30H
        MOV AL,COMMAND
        ;....................................
        NEXT_STEP_1:
        
        CMP AL,30H
        JE GRAMMI_0_X
        
        CMP AL,31H
        JE GRAMMI_1_X
        
        CMP AL,32H
        JE GRAMMI_2_X
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        JMP LOOP_INPUT_X
        ;....................................
        PUSHBACKSPACE:
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV AL," "
        CALL CHAR_OUTPUT
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        ;....................................
        JMP LOOP_INPUT_X
        
        ;.............................
        ;.............................
        ;.............................
        
        GRAMMI_0_X:
        
        LOOP_0:
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CMP HOW_MANY,2
        JE NEXT_STEP1 
        
        CMP TEMP,1
        JE NEXT_STEP1
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_2R
         
        NEXT_STEP1:
        
        CMP DRAW,8
        JE AUTOMOVE2
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JE PUSHBACKSPACE
        
        CMP AL,30H
        JB LOOP_0
        CMP AL,32H
        JA LOOP_0
        
        MOV BH,AL
        
        MOV BL,0CH
        CALL CHAR_OUTPUT
        
        CALL ENTER_MESSAGE
        ;....................................
        BACKORCONT:
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JNE GOGO
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV AL," "
        CALL CHAR_OUTPUT
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CALL CLEARING_MESSAGE
        
        JMP LOOP_0
        
        GOGO:
        
        CMP AL,13
        JE NEXT_STEP_2
        
        JMP BACKORCONT
        ;....................................
        AUTOMOVE2:
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_2R
        ;.................................... 
        NEXT_STEP_2:
        
        MOV AL,BH 
        
        NEXT_STEP_2R:
         
        CMP AL,30H
        JE STILI_0_X
        
        CMP AL,31H
        JE STILI_1_X
        
        CMP AL,32H
        JE STILI_2_X
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        JMP LOOP_0
        
        ;.............................
        
        STILI_0_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[0],1
        JNE A
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        A: 
        CMP BUFFER[0],1
        JNE B
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        B:
        MOV CX,105 ;arxikopoihsh grammh
        MOV DX,45 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,62 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[0],1
 
        JMP TELOS
        
        ;.............................
        
        STILI_1_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[1],1
        JNE C
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        C: 
        CMP BUFFER[1],1
        JNE D
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        D:
        
        MOV CX,147 ;arxikopoihsh grammh
        MOV DX,45 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,62 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[1],1
        
        JMP TELOS
        
        ;.............................
        
        STILI_2_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[2],1
        JNE E
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        E: 
        CMP BUFFER[2],1
        JNE F
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        F:
        
        MOV CX,188 ;arxikopoihsh grammh
        MOV DX,45 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,62 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[2],1
        
        JMP TELOS
        
        ;.............................
        ;.............................
        ;.............................
        
        GRAMMI_1_X:
        
        LOOP_1:
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CMP HOW_MANY,2
        JE NEXT_STEP2
        
        CMP TEMP,1
        JE NEXT_STEP2 
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_3R
         
        NEXT_STEP2:
        
        CMP DRAW,8
        JE AUTOMOVE3
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JE PUSHBACKSPACE
        
        CMP AL,30H
        JB LOOP_1
        CMP AL,32H
        JA LOOP_1
        
        MOV BH,AL
        
        MOV BL,0CH
        CALL CHAR_OUTPUT
        
        CALL ENTER_MESSAGE
        ;....................................
        BACKORCONT1:
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JNE GOGO1
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV AL," "
        CALL CHAR_OUTPUT
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CALL CLEARING_MESSAGE
        
        JMP LOOP_1
        
        GOGO1:
        
        CMP AL,13
        JE NEXT_STEP_3
        
        JMP BACKORCONT1
        ;....................................
        AUTOMOVE3:
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_3R
        
        ;.................................... 
        NEXT_STEP_3:
        
        MOV AL,BH
        
        NEXT_STEP_3R:
        
        CMP AL,30H
        JE STILI2_0_X
        
        CMP AL,31H
        JE STILI2_1_X
        
        CMP AL,32H
        JE STILI2_2_X
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        ;....................................
        
        JMP LOOP_1
        
        ;....................................
        
        
        STILI2_0_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[3],1
        JNE G
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        G: 
        CMP BUFFER[3],1
        JNE H
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        H:
        
        MOV CX,105 ;arxikopoihsh grammh
        MOV DX,75 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,92 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[3],1
        
        JMP TELOS
        
        ;.............................
        
        STILI2_1_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[4],1
        JNE I
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        I: 
        CMP BUFFER[4],1
        JNE J
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        J:
        
        MOV CX,148 ;arxikopoihsh grammh
        MOV DX,75 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,92 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[4],1
        
        JMP TELOS
        
        ;.............................
        
        
        STILI2_2_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[5],1
        JNE K
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        K: 
        CMP BUFFER[5],1
        JNE L
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        L:
        
        MOV CX,187 ;arxikopoihsh grammh
        MOV DX,75 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,92 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[5],1
        
        JMP TELOS
        
        ;.............................
        ;.............................
        ;.............................
        
        GRAMMI_2_X:
        
        LOOP_2:
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CMP HOW_MANY,2
        JE NEXT_STEP3
        
        CMP TEMP,1
        JE NEXT_STEP3
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_4R
         
        NEXT_STEP3:
        
        CMP DRAW,8
        JE AUTOMOVE4
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JE PUSHBACKSPACE
        
        CMP AL,30H
        JB LOOP_2
        CMP AL,32H
        JA LOOP_2
        
        MOV BH,AL
        
        MOV BL,0CH
        CALL CHAR_OUTPUT
        
        CALL ENTER_MESSAGE
        ;....................................
        BACKORCONT2:
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JNE GOGO2
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV AL," "
        CALL CHAR_OUTPUT
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CALL CLEARING_MESSAGE
        
        JMP LOOP_2
        
        GOGO2:
        
        CMP AL,13
        JE NEXT_STEP_4
        
        JMP BACKORCONT2
        ;....................................
        AUTOMOVE4:
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_4R
        
        ;.................................... 
        NEXT_STEP_4:
        
        MOV AL,BH
        
        NEXT_STEP_4R:
        
        CMP AL,30H
        JE STILI3_0_X
        
        CMP AL,31H
        JE STILI3_1_X
        
        CMP AL,32H
        JE STILI3_2_X
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        ;....................................
        
        JMP LOOP_2
        
        ;....................................
        
        STILI3_0_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[6],1
        JNE M
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        M: 
        CMP BUFFER[6],1
        JNE N
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        N:
        
        MOV CX,105 ;arxikopoihsh grammh
        MOV DX,105 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,122 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[6],1
        
        JMP TELOS
        
        ;.............................
        
        STILI3_1_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[7],1
        JNE O
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        O: 
        CMP BUFFER[7],1
        JNE P
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        P:
        
        MOV CX,147 ;arxikopoihsh grammh
        MOV DX,105 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,122 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[7],1
        
        JMP TELOS
        
        ;.............................
        
        STILI3_2_X:
        
        CALL REVALUE 
        
        CMP BUFFER2[8],1
        JNE Q
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X 
        Q: 
        CMP BUFFER[8],1
        JNE R
        CALL INVALID_VALUE
        JMP LOOP_INPUT_X
        R:
        
        MOV CX,188 ;arxikopoihsh grammh
        MOV DX,105 ;arxikopoihsh sthlhs
        CALL X_DRAW_1
        
        MOV DX,122 ;arxikopoihsh sthlhs
        CALL X_DRAW_2
        
        MOV BUFFER[8],1
        
        TELOS:
        
        INC DRAW
        
        POP CX
        POP DX
        POP AX
        
        RET
    
INPUT_X ENDP
;---------------------------------------------
REVALUE PROC   ;Sto pedio twn timwn x kai y ean uparxoun xaraktires eksafanizodai /Katharismos tou pediou
     
     PUSH AX
     PUSH DX
     
     MOV DL,35
     MOV DH,1
     CALL CURSOR_POSITION
        
     MOV AL," "
     CALL CHAR_OUTPUT
     
     MOV DL,36
     MOV DH,1
     CALL CURSOR_POSITION
        
     MOV AL," "
     CALL CHAR_OUTPUT
        
     MOV DL,35
     MOV DH,1
     CALL CURSOR_POSITION 
     
     POP DX
     POP AX
     
     RET
REVALUE ENDP    
;---------------------------------------------

INPUT_O PROC     ;Eisagwgi sidetagmenwn gia to "O"
    
        PUSH AX
        PUSH DX
        PUSH CX
        
        ;Omoia i logiki opws stin "INPUT_X"
        
        LOOP_INPUT_O:
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        CMP HOW_MANY,2
        JE NEXT_STEP_O
        
        CMP TEMP,2
        JE NEXT_STEP_O
        
        CALL RANDOM_COMMAND
        ADD COMMAND,30H
        ADD COMMAND2,30H
        MOV AL,COMMAND
        JMP NEXT_STEP_O_1
         
        NEXT_STEP_O:
        
        CMP DRAW,8
        JE AUTOMOVE5
        
        MOV AH,08
        INT 21H
        
        CALL CLEARING_MESSAGE
         
        CMP AL,30H
        JB LOOP_INPUT_O
        CMP AL,32H
        JA LOOP_INPUT_O
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV BL,0CH
        CALL CHAR_OUTPUT
        
        JMP NEXT_STEP_O_1
        
        AUTOMOVE5:
        
        CALL RANDOM_COMMAND
        ADD COMMAND,30H
        ADD COMMAND2,30H
        MOV AL,COMMAND

        NEXT_STEP_O_1:
        
        CMP AL,30H                 
        JE GRAMMI_0_O
        
        CMP AL,31H
        JE GRAMMI_1_O
        
        CMP AL,32H
        JE GRAMMI_2_O
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        JMP LOOP_INPUT_O
        
        PUSHBACKSPACE2:
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV AL," "
        CALL CHAR_OUTPUT
        
        MOV DL,35
        MOV DH,1
        CALL CURSOR_POSITION
        
        JMP LOOP_INPUT_O
        
        ;.............................
        ;.............................
        ;.............................
        
        GRAMMI_0_O:
        
        
        LOOP_STILI_0_O:
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CMP HOW_MANY,2
        JE NEXT_STEP_O1
        
        CMP TEMP,2
        JE NEXT_STEP_O1
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_O_2R
         
        NEXT_STEP_O1:
        
        CMP DRAW,8
        JE AUTOMOVE6
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JE PUSHBACKSPACE2
        
        CMP AL,30H
        JB LOOP_STILI_0_O
        CMP AL,32H
        JA LOOP_STILI_0_O
        
        MOV BH,AL
        
        MOV BL,0CH
        CALL CHAR_OUTPUT
        
        CALL ENTER_MESSAGE
        ;....................................
        BACKORCONT3:
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JNE GOGO3
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV AL," "
        CALL CHAR_OUTPUT
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CALL CLEARING_MESSAGE
        
        JMP LOOP_STILI_0_O
        
        GOGO3:
        
        CMP AL,13
        JE NEXT_STEP_O_2
        
        JMP BACKORCONT3 
        ;....................................
        AUTOMOVE6:
        
        MOV AL,COMMAND2
        
        JMP NEXT_STEP_O_2R     
        
        NEXT_STEP_O_2:
        
        MOV AL,BH
        
        NEXT_STEP_O_2R:
        
        CMP AL,30H
        JE STILI_0_O
        
        CMP AL,31H
        JE STILI_1_O
        
        CMP AL,32H
        JE STILI_2_O
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        JMP LOOP_STILI_0_O:
        
        ;.............................
        
        STILI_0_O:
        
        CALL REVALUE
        
        CMP BUFFER[0],1
        JNE AA
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        AA:
        CMP BUFFER2[0],1
        JNE BB
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        BB:
        
        MOV CX,104 ;arxikopoihsh grammh
        MOV BUFFER2[0],1
        
        MOV DX,52 ;arxikopoihsh sthlhs
        
        JMP DRAW_CIRCLE
        
        ;.............................
        
        STILI_1_O:
        
        CALL REVALUE
        
        CMP BUFFER[1],1
        JNE CC
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        CC:
        CMP BUFFER2[1],1
        JNE DDD
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        DDD:
        
        MOV CX,146 ;arxikopoihsh grammh
        MOV BUFFER2[1],1 
        
        MOV DX,52 ;arxikopoihsh sthlhs
        
        JMP DRAW_CIRCLE
        
        ;.............................
        
        STILI_2_O:
        
        CALL REVALUE
        
        CMP BUFFER[2],1
        JNE EE
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        EE:
        CMP BUFFER2[2],1
        JNE FF
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        FF:
        
        MOV CX,187 ;arxikopoihsh grammh
        MOV BUFFER2[2],1 
        
        MOV DX,52 ;arxikopoihsh sthlhs
        
        JMP DRAW_CIRCLE
        
        ;.............................
        ;.............................
        ;.............................
        
        GRAMMI_1_O:
        
        LOOP_10:
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CMP HOW_MANY,2
        JE NEXT_STEP_O2
        
        CMP TEMP,2
        JE NEXT_STEP_O2
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_O_3R
         
        NEXT_STEP_O2:
        
        CMP DRAW,8
        JE AUTOMOVE7
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JE PUSHBACKSPACE2
        
        CMP AL,30H
        JB LOOP_10
        CMP AL,32H
        JA LOOP_10
        
        MOV BH,AL
        
        MOV BL,0CH
        CALL CHAR_OUTPUT
        CALL ENTER_MESSAGE
        ;....................................
        BACKORCONT4:
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JNE GOGO4
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV AL," "
        CALL CHAR_OUTPUT
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CALL CLEARING_MESSAGE
        
        JMP LOOP_10
        
        GOGO4:
        
        CMP AL,13
        JE NEXT_STEP_O_3
        
        JMP BACKORCONT4 
        ;....................................
        
        AUTOMOVE7:
        
        MOV AL,COMMAND2 
        
        JMP NEXT_STEP_O_3R
        
        NEXT_STEP_O_3:
        
        MOV AL,BH
        
        NEXT_STEP_O_3R:
        
        CMP AL,30H
        JE STILI2_0_O
        
        CMP AL,31H
        JE STILI2_1_O
        
        CMP AL,32H
        JE STILI2_2_O
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        JMP LOOP_10
        
        ;.............................
        
        STILI2_0_O:
        
        CALL REVALUE
        
        CMP BUFFER[3],1
        JNE GG
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        GG:
        CMP BUFFER2[3],1
        JNE HH
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        HH:
        
        MOV CX,104 ;arxikopoihsh grammh
        MOV BUFFER2[3],1 
        
        MOV DX,82 ;arxikopoihsh sthlhs
        
        JMP DRAW_CIRCLE
        
        ;.............................
        
        STILI2_1_O:
        
        CALL REVALUE
        
        CMP BUFFER[4],1
        JNE II
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        II:
        CMP BUFFER2[4],1
        JNE JJ
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        JJ:
        
        MOV CX,146 ;arxikopoihsh grammh
        MOV BUFFER2[4],1 
        
        MOV DX,82 ;arxikopoihsh sthlhs
        
        JMP DRAW_CIRCLE
        
        ;.............................    
        
        STILI2_2_O: 
        
        CALL REVALUE
        
        CMP BUFFER[5],1
        JNE KK
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        KK:
        CMP BUFFER2[5],1
        JNE LL
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        LL:
        
        MOV CX,187 ;arxikopoihsh grammh
        MOV BUFFER2[5],1
        
        MOV DX,82 ;arxikopoihsh sthlhs
        
        JMP DRAW_CIRCLE
        
        ;.............................
        ;.............................
        ;.............................
        
        GRAMMI_2_O:
             
        LOOP_20:
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CMP HOW_MANY,2
        JE NEXT_STEP_O3
        
        CMP TEMP,2
        JE NEXT_STEP_O3
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_O_4R
         
        NEXT_STEP_O3:
        
        CMP DRAW,8
        JE AUTOMOVE8
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JE PUSHBACKSPACE2
        
        CMP AL,30H
        JB LOOP_20
        CMP AL,32H
        JA LOOP_20
        
        MOV BH,AL
        
        MOV BL,0CH
        CALL CHAR_OUTPUT
        CALL ENTER_MESSAGE
        ;....................................
        BACKORCONT5:
        
        MOV AH,08
        INT 21H
        
        CMP AL,8
        JNE GOGO5
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        MOV AL," "
        CALL CHAR_OUTPUT
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        CALL CLEARING_MESSAGE
        
        JMP LOOP_20
        
        GOGO5:
        
        CMP AL,13
        JE NEXT_STEP_O_4
        
        JMP BACKORCONT5 
        ;....................................
        AUTOMOVE8:
        
        MOV AL,COMMAND2
        JMP NEXT_STEP_O_4R
        
        NEXT_STEP_O_4:
        
        MOV AL,BH
        
        NEXT_STEP_O_4R:
        
        CMP AL,30H
        JE STILI3_0_O
        
        CMP AL,31H
        JE STILI3_1_O
        
        CMP AL,32H
        JE STILI3_2_O
        
        MOV DL,36
        MOV DH,1
        CALL CURSOR_POSITION
        
        JMP LOOP_20
        
        ;.............................
        
        STILI3_0_O:
        
        CALL REVALUE
        
        CMP BUFFER[6],1
        JNE MM
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        MM:
        CMP BUFFER2[6],1
        JNE NN
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        NN:
        
        MOV CX,104 ;arxikopoihsh grammh
        MOV BUFFER2[6],1
        
        MOV DX,112 ;arxikopoihsh sthlhs
        
        JMP DRAW_CIRCLE
        
        ;.............................
        
        STILI3_1_O:
        
        CALL REVALUE
        
        CMP BUFFER[7],1
        JNE OO
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        OO:
        CMP BUFFER2[7],1
        JNE PP
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        PP:
        
        MOV CX,146 ;arxikopoihsh grammh
        MOV BUFFER2[7],1
        
        MOV DX,112 ;arxikopoihsh sthlhs
        
        JMP DRAW_CIRCLE
        
        ;.............................
        
        STILI3_2_O:
        
        CALL REVALUE
        
        CMP BUFFER[8],1
        JNE QQ
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        QQ:
        CMP BUFFER2[8],1
        JNE RR
        CALL INVALID_VALUE
        JMP LOOP_INPUT_O
        RR:
        
        MOV CX,187 ;arxikopoihsh grammh
        MOV BUFFER2[8],1
        
        MOV DX,112 ;arxikopoihsh sthlhs
        
        DRAW_CIRCLE:
        
        INC DRAW
        
        CALL CIRCLE
        
        POP CX
        POP DX
        POP AX
        
        RET
    
INPUT_O ENDP
;-----------------------------------------
DRAW_CHECK PROC     ;Elegxos sunolou kinisewn gia tin isopalia
    
    PUSH AX
    PUSH BX
    PUSH DX
    PUSH CX
    PUSH SI
    
    CMP DRAW,9
    JNE TELOSDRAW
    
    ;.....................................
    
    CALL GRAPH
    
    ;.....................................
    
    CALL TAF 
    CALL IOT 
    CALL EPSILON
    CALL ALPHA1
    CALL OMICRON
    CALL CI
    CALL PAULA
    
    ;.....................................
    
    MOV DL,17
    MOV DH,12
    CALL CURSOR_POSITION
    
    XOR SI,SI
    MOV CX,5
    DRAWMESSAGE:  ;Vroxos emfanisis minimatos isopalias
    MOV AL,DRAWmsg[SI]
    MOV BL,0DH
    CALL CHAR_OUTPUT
    INC SI
    INC DL
    CALL CURSOR_POSITION
    LOOP DRAWMESSAGE 
    
    ;.....................................
    
    MOV DL,14
    MOV DH,15
    CALL CURSOR_POSITION
    
    MOV CX,11
    XOR SI,SI
    
    AGAIN_MSG2:        ;Vroxos emfanisis minimatos gia to an thelei na paiksei ksana o paiktis
    MOV AL,AGAINMSG[SI]
    MOV BL,8H
    CALL CHAR_OUTPUT
    INC SI
    INC DL
    CALL CURSOR_POSITION
    LOOP AGAIN_MSG2
    
    ;.....................................
    
    CALL KEY2
    
    ;.....................................
    
    GIVEANS:
    MOV AH,8H
    INT 21H
    CMP AL,59H
    JE GO
    CMP AL,4EH
    JNE GIVEANS
    GO:
    MOV FORES,AL
    
    ;.....................................
    
    MOV PLAYER,3      ;Dinetai i timi 3 gia na min dwthei podos se kanenan paikti stin "ARCHIVE" fainetai i xrisi
    MOV DI,1
    
    ;.....................................
    
    TELOSDRAW:
    
    POP SI
    POP CX
    POP DX
    POP BX
    POP AX
    
    RET
DRAW_CHECK ENDP
;-----------------------------------------
CHAR_OUTPUT PROC   ;Sunartisi gia tin emfanisi xaraktira
    
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    MOV AH,09H
    MOV BH,0
    
    MOV CX,1
    INT 10H
    
    POP DX
    POP CX
    POP BX
    POP AX
    
    RET
    
CHAR_OUTPUT ENDP
;-----------------------------------------
CURSOR_POSITION PROC    ;Sunartisi gia tin topothetisi tou kersora
        
        PUSH AX
        PUSH BX
        
        
        MOV BH,0
        MOV AH,2
        INT 10H
        
        POP BX
        POP AX
        
        RET
        
CURSOR_POSITION ENDP
;-----------------------------------------
CIRCLE PROC          ;Emfanisi kiklou
        
         PUSH AX
         PUSH DX
         PUSH BX
         PUSH SI
         PUSH DI
         
         ;Oi times exoun upologistei pio prin kai einai sigkekrimenes
         
         XOR DI,DI
         XOR BX,BX
         ;..............................................
         MOV SI,4
         MOV AX,CX
         MOV BX,DX
         CALL PIXEL_1
         ;..............................................
         ADD CX,19
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         INC CX
         MOV SI,2
         SUB DX,2
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,18
         MOV SI,2
         SUB DX,2
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         INC CX
         MOV SI,2
         ADD DX,4
         ;..............................................
         CALL PIXEL_1
         MOV CX,AX
         MOV DX,BX
         ADD CX,18
         MOV SI,2
         ADD DX,4
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,2
         SUB DX,4
         MOV SI,2
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,3
         SUB DX,5
         MOV SI,1
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,2
         ADD DX,6
         MOV SI,2
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,3
         ADD DX,8
         MOV SI,1
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,17
         SUB DX,4
         MOV SI,2
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,17
         ADD DX,6
         MOV SI,2
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,16
         SUB DX,5
         MOV SI,1
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,16
         ADD DX,8
         MOV SI,1
         CALL PIXEL_1
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,14
         ADD DX,9
         MOV SI,2
         CALL PIXEL_2
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,12
         ADD DX,10
         MOV SI,2
         CALL PIXEL_2
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,8
         ADD DX,11
         MOV SI,4
         CALL PIXEL_2
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,4
         ADD DX,9
         MOV SI,2
         CALL PIXEL_2
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,6
         ADD DX,10
         MOV SI,2
         CALL PIXEL_2
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,4
         SUB DX,6
         MOV SI,2
         CALL PIXEL_2
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,6
         SUB DX,7
         MOV SI,2
         CALL PIXEL_2
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,12
         SUB DX,7
         MOV SI,2
         CALL PIXEL_2  
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,14
         SUB DX,6
         MOV SI,2
         CALL PIXEL_2
         ;..............................................
         MOV CX,AX
         MOV DX,BX
         ADD CX,8
         SUB DX,8
         MOV SI,4
         CALL PIXEL_2
         
         POP DI
         POP SI
         POP BX
         POP DX
         POP AX
         
         RET
        
CIRCLE ENDP

;.............................................
      
PIXEL_1 PROC        ;Sunartisi gia ton sxediasmo grammis katheta
        
        PUSH CX
        PUSH BX
        PUSH DX
        PUSH AX
        PUSH DI
        
        P_LOOP:
        
        MOV AH,0CH
        MOV AL,4
        INT 10H
        INC DX
        INC DI
        CMP DI,SI
        
        JB P_LOOP
        
        POP DI
        POP AX
        POP DX
        POP BX
        POP CX
        
        RET
      
PIXEL_1 ENDP 

;.............................................
      
PIXEL_2 PROC        ;Sunartisi gia ton sxediasmo grammis orizodia
        
        PUSH CX
        PUSH BX
        PUSH DX
        PUSH AX
        PUSH DI
        
        P_LOOP2:
        
        MOV AH,0CH
        MOV AL,4
        INT 10H
        INC CX
        INC DI
        CMP DI,SI
        
        JB P_LOOP2
        
        POP DI
        POP AX
        POP DX
        POP BX
        POP CX
        
        RET
      
PIXEL_2 ENDP
;-------------------------------------------
RANDOM_COMMAND PROC

        PUSH AX
        PUSH DX
        PUSH CX 
        PUSH BX
        PUSH SI
        
        ;H logiki einai ean einai 1os o upologistis paizei se gwnies kai otan boresei siblirwnwei to keli pou prepei gia na kanei triliza
        ;Ean einai deuteros analoga me tin kinisi tou 1ou paikti vazei se gwnia i kedro - auto ginetai mono mia fora meta sinexizei se gwnies
        ;h siblirwsi trilizas
        
        CMP HOW_MANY,2
        JE FIRST_MOVE
        
        CMP TEMP,1
        JE  TIMING
         
        JMP FIRST_MOVE
        
        TIMING:
        
        CMP TIMES2,0
        JE SPECIAL_CHECK
        
        JMP FIRST_MOVE
        
        SPECIAL_CHECK:
        
        CMP BUFFER[1],1
        JE CORNER_MOVE
        
        CMP BUFFER[3],1
        JE CORNER_MOVE
        
        CMP BUFFER[5],1
        JE CORNER_MOVE
        
        CMP BUFFER[7],1
        JE CORNER_MOVE
        
        JMP SQUARE_4 
        
        FIRST_MOVE:
        
        XOR BX,BX
        
        CALL SMARTCHOICE
        MOV BL,AL 
        CMP DI,2
        JE CHECK_IN
        
        CORNER_MOVE:
        
        MOV AH,0
        INT 1AH
        
        AND DX,0011B
        MOV BL,DL

        CMP BL,4
        JA CORNER_MOVE
        
        CMP BL,0
        JE SQUARE_0
        
        CMP BL,1
        JE SQUARE_2
        
        CMP BL,2
        JE SQUARE_6
        
        CMP BL,3
        JE SQUARE_8
         
        
        ;............................................
        ;............................................ 
        ;............................................
        ;............................................ 
        ;............................................
        
        ;Analoga me to keli pou proekipse oti prepei na paei ginodai oi parakatw elegxei to keli prokeiptei apo tin "SMARTCHOICE" kai 
        ;dinetai apo ton AL ston BL gia na ginei i sigkrisi
        
        CHECK_IN:
        
        CMP BL,0
        JE SQUARE_0
        
        CMP BL,1
        JE SQUARE_1
        
        CMP BL,2
        JE SQUARE_2
        
        CMP BL,3
        JE SQUARE_3
        
        CMP BL,4
        JE SQUARE_4
        
        CMP BL,5
        JE SQUARE_5
        
        CMP BL,6
        JE SQUARE_6
        
        CMP BL,7
        JE SQUARE_7
        
        CMP BL,8
        JE SQUARE_8
        
        JMP CORNER_MOVE
        
        ;................................................
        
        ;Oi sudetagmenes dinodai automata analoga me to keli pou prepei na paei
        
        SQUARE_0:
        
        MOV COMMAND,0
        MOV COMMAND2,0
        
        JMP END_RAND
        
        SQUARE_1:
        
        MOV COMMAND,0
        MOV COMMAND2,1
        
        JMP END_RAND
        
        SQUARE_2:
        
        MOV COMMAND,0
        MOV COMMAND2,2
        
        JMP END_RAND
        
        SQUARE_3:
        
        MOV COMMAND,1
        MOV COMMAND2,0
        
        JMP END_RAND
        
        SQUARE_4:
        
        MOV COMMAND,1
        MOV COMMAND2,1
        
        JMP END_RAND
        
        SQUARE_5:
        
        MOV COMMAND,1
        MOV COMMAND2,2
        
        JMP END_RAND
        
        SQUARE_6:
        
        MOV COMMAND,2
        MOV COMMAND2,0
        
        JMP END_RAND
        
        SQUARE_7:
        
        MOV COMMAND,2
        MOV COMMAND2,1
        
        JMP END_RAND 
        
        SQUARE_8:
        
        MOV COMMAND,2
        MOV COMMAND2,2
         
        END_RAND:
        
        INC TIMES2
        
        POP SI
        POP BX
        POP CX
        POP DX
        POP AX
        
        RET 
        
RANDOM_COMMAND ENDP
;------------------------------------------- 
SMARTCHOICE PROC    ;Epilogi tis kaliteris kinisis
        
        PUSH CX
        PUSH SI
        PUSH BX
        
        ;Paromoia logiki me twn elegxo gia nikiti
        ;To DI pairnei tin timi 5 otan prokipei stin triliza pou exetazei na min einai swsti-na periexei diaforetiko sumvolo apo oti prepei
        ;Se periptwsi pou den einai ksekatharo pou prepei na topothetithei to sumvolo tou upologisti, kratietai to teleutaio keli pou vrike 
        ;keno
        ;Xrisimopoiitai gia tin teleutaia automatopoiimeni kinisi
        
        CMP TEMP,1
        JE SMART_O
        
        XOR DI,DI
        XOR BX,BX
        
        MOV CX,3
        XOR SI,SI
        XOR AX,AX
        
        TOTAL_HORIZONTAL_ATTACK:
        
        PUSH CX
        
        MOV CX,3
    
        HORIZONTAL_CHECK_ATTACK:
                             
        CMP BUFFER[SI],1
        JE INC_COUNT_ATTACK 
        
        CMP BUFFER2[SI],1
        JE  COUNT5_ATTACK
        
        MOV AX,SI
         
        JMP NEXT_CHECK_ATTACK
        
        COUNT5_ATTACK:
        MOV DI,5
        
        JMP NEXT_CHECK_ATTACK
        
        INC_COUNT_ATTACK:
        INC DI
        
        NEXT_CHECK_ATTACK:                    
        
        INC SI
                             
        LOOP HORIZONTAL_CHECK_ATTACK
        
        POP CX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP TOTAL_HORIZONTAL_ATTACK
        
;-------------------------------------
        XOR DI,DI
        
        MOV CX,3
        XOR SI,SI
        
        TOTAL_HORIZONTAL_DEFENCE:
        
        PUSH CX
        
        MOV CX,3
    
        HORIZONTAL_CHECK_DEFENCE:
                             
        CMP BUFFER2[SI],1
        JE INC_COUNT_DEFENCE 
        
        CMP BUFFER[SI],1
        JE  COUNT5_DEFENCE
        
        MOV AX,SI
         
        JMP NEXT_CHECK_DEFENCE
        
        COUNT5_DEFENCE:
        MOV DI,5
        
        JMP NEXT_CHECK_DEFENCE
        
        INC_COUNT_DEFENCE:
        INC DI
        
        NEXT_CHECK_DEFENCE:                    
        
        INC SI
                             
        LOOP HORIZONTAL_CHECK_DEFENCE
        
        POP CX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI     
              
        LOOP TOTAL_HORIZONTAL_DEFENCE
;------------------------------------------- 
        XOR DI,DI
        XOR BX,BX

        MOV CX,3
        XOR SI,SI
        
        TOTAL_VERTICAL_ATTACK:
        
        PUSH CX
        
        MOV CX,3
    
        VERTICAL_CHECK_ATTACK:
                             
        CMP BUFFER[SI],1
        JE INC_COUNT_ATTACKV 
        
        CMP BUFFER2[SI],1
        JE  COUNT5_ATTACKV
        
        MOV AX,SI
         
        JMP NEXT_CHECK_ATTACKV
        
        COUNT5_ATTACKV:
        MOV DI,5
        
        JMP NEXT_CHECK_ATTACKV
        
        INC_COUNT_ATTACKV:
        INC DI
        
        NEXT_CHECK_ATTACKV:                    
        
        ADD SI,3
                             
        LOOP VERTICAL_CHECK_ATTACK
        
        POP CX
        
        XOR SI,SI
        
        INC BX
        
        ADD SI,BX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP TOTAL_VERTICAL_ATTACK
;----------------------------------------------
        XOR DI,DI
        XOR BX,BX
        
        MOV CX,3
        XOR SI,SI
        
        TOTAL_VERTICAL_DEFENCE:
        
        PUSH CX
        
        MOV CX,3
    
        VERTICAL_CHECK_DEFENCE:
                             
        CMP BUFFER2[SI],1
        JE INC_COUNT_DEFENCE_VERTICAL 
        
        CMP BUFFER[SI],1
        JE  COUNT5_DEFENCE_VERTICAL
        
        MOV AX,SI
         
        JMP NEXT_CHECK_DEFENCE_VERTICAL
        
        COUNT5_DEFENCE_VERTICAL:
        MOV DI,5
        
        JMP NEXT_CHECK_DEFENCE_VERTICAL
        
        INC_COUNT_DEFENCE_VERTICAL:
        INC DI
        
        NEXT_CHECK_DEFENCE_VERTICAL:                    
        
        ADD SI,3
                           
        LOOP VERTICAL_CHECK_DEFENCE
        
        POP CX
        
        XOR SI,SI
              
        INC BX
        
        ADD SI,BX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP TOTAL_VERTICAL_DEFENCE
        
        ;----------------------------------------------
        
        XOR DI,DI
        XOR BX,BX
        MOV BX,4
        MOV CX,2
        XOR SI,SI
        
        DIAGONAL_CHECK_ATTACK_MAIN:
        
        PUSH CX
        
        MOV CX,3
        
        DIAGONAL_CHECK_ATTACK:
                             
        CMP BUFFER[SI],1
        JE INC_COUNT_ATTACK_DIAGONAL 
        
        CMP BUFFER2[SI],1
        JE  COUNT5_ATTACK_DIAGONAL
        
        MOV AX,SI
         
        JMP NEXT_CHECK_ATTACK_DIAGONAL
        
        COUNT5_ATTACK_DIAGONAL:
        MOV DI,5
        
        JMP NEXT_CHECK_ATTACK_DIAGONAL
        
        INC_COUNT_ATTACK_DIAGONAL:
        INC DI
        
        NEXT_CHECK_ATTACK_DIAGONAL:                    
        
        ADD SI,BX
                           
        LOOP DIAGONAL_CHECK_ATTACK
        
        POP CX
        
        XOR SI,SI
        XOR BX,BX      
        MOV BX,2
        MOV SI,BX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP DIAGONAL_CHECK_ATTACK_MAIN
        
        ;----------------------------------------------
        
        XOR DI,DI
        XOR BX,BX
        MOV BX,4
        MOV CX,2
        XOR SI,SI
        
        DIAGONAL_CHECK_DEFENSE_MAIN:
        
        PUSH CX
        
        MOV CX,3
        
        DIAGONAL_CHECK_DEFENSE:
                             
        CMP BUFFER2[SI],1
        JE INC_COUNT_DEFENSE_DIAGONAL 
        
        CMP BUFFER[SI],1
        JE  COUNT5_DEFENSE_DIAGONAL
        
        MOV AX,SI
         
        JMP NEXT_CHECK_DEFENSE_DIAGONAL
        
        COUNT5_DEFENSE_DIAGONAL:
        MOV DI,5
        
        JMP NEXT_CHECK_DEFENSE_DIAGONAL
        
        INC_COUNT_DEFENSE_DIAGONAL:
        INC DI
        
        NEXT_CHECK_DEFENSE_DIAGONAL:                    
        
        ADD SI,BX
                           
        LOOP DIAGONAL_CHECK_DEFENSE
        
        POP CX
        
        XOR SI,SI
        XOR BX,BX      
        MOV BX,2
        MOV SI,BX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP DIAGONAL_CHECK_DEFENSE_MAIN 
        
        ;----------------------------------------------
        
        SMART_O:
        
        XOR DI,DI
        
        MOV CX,3
        XOR SI,SI
        
        TOTAL_HORIZONTAL_ATTACK_O:
        
        PUSH CX
        
        MOV CX,3
    
        HORIZONTAL_CHECK_ATTACK_O:
                             
        CMP BUFFER2[SI],1
        JE INC_COUNT_ATTACK_O 
        
        CMP BUFFER[SI],1
        JE COUNT5_ATTACK_O
        
        MOV AX,SI
         
        JMP NEXT_CHECK_ATTACK_O
        
        COUNT5_ATTACK_O:
        MOV DI,5
        
        JMP NEXT_CHECK_ATTACK_O
        
        INC_COUNT_ATTACK_O:
        INC DI
        
        NEXT_CHECK_ATTACK_O:                    
        
        INC SI
                             
        LOOP HORIZONTAL_CHECK_ATTACK_O
        
        POP CX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI     
              
        LOOP TOTAL_HORIZONTAL_ATTACK_O
;---------------------------------------------------
        XOR DI,DI
        XOR BX,BX
        
        MOV CX,3
        XOR SI,SI
        XOR AX,AX
        
        TOTAL_HORIZONTAL_DEFENCE_O:
        
        PUSH CX
        
        MOV CX,3
    
        HORIZONTAL_CHECK_DEFENCE_O:
                             
        CMP BUFFER[SI],1
        JE INC_COUNT_DEFENCE_O 
        
        CMP BUFFER2[SI],1
        JE  COUNT5_DEFENCE_O
        
        MOV AX,SI
         
        JMP NEXT_CHECK_DEFENCE_O
        
        COUNT5_DEFENCE_O:
        MOV DI,5
        
        JMP NEXT_CHECK_DEFENCE_O
        
        INC_COUNT_DEFENCE_O:
        INC DI
        
        NEXT_CHECK_DEFENCE_O:                    
        
        INC SI
                             
        LOOP HORIZONTAL_CHECK_DEFENCE_O
        
        POP CX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP TOTAL_HORIZONTAL_DEFENCE_O
        
;-------------------------------------
        XOR DI,DI
        XOR BX,BX
        
        MOV CX,3
        XOR SI,SI
        
        TOTAL_VERTICAL_ATTACKV_O:
        
        PUSH CX
        
        MOV CX,3
    
        VERTICAL_CHECK_ATTACKV_O:
                             
        CMP BUFFER2[SI],1
        JE INC_COUNT_ATTACKV_O_VERTICAL 
        
        CMP BUFFER[SI],1
        JE  COUNT5_ATTACKV_O_VERTICAL
        
        MOV AX,SI
         
        JMP NEXT_CHECK_ATTACKV_O_VERTICAL
        
        COUNT5_ATTACKV_O_VERTICAL:
        MOV DI,5
        
        JMP NEXT_CHECK_ATTACKV_O_VERTICAL
        
        INC_COUNT_ATTACKV_O_VERTICAL:
        INC DI
        
        NEXT_CHECK_ATTACKV_O_VERTICAL:                    
        
        ADD SI,3
                           
        LOOP VERTICAL_CHECK_ATTACKV_O
        
        POP CX
        
        XOR SI,SI
              
        INC BX
        
        ADD SI,BX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP TOTAL_VERTICAL_ATTACKV_O
        
        ;----------------------------------------------
        XOR DI,DI
        XOR BX,BX

        MOV CX,3
        XOR SI,SI
        
        TOTAL_VERTICAL_DEFENCEV_O:
        
        PUSH CX
        
        MOV CX,3
    
        VERTICAL_CHECK_DEFENCEV_O:
                             
        CMP BUFFER[SI],1
        JE INC_COUNT_DEFENCEV_O 
        
        CMP BUFFER2[SI],1
        JE  COUNT5_DEFENCEV_O
        
        MOV AX,SI
         
        JMP NEXT_CHECK_DEFENCEV_O
        
        COUNT5_DEFENCEV_O:
        MOV DI,5
        
        JMP NEXT_CHECK_DEFENCEV_O
        
        INC_COUNT_DEFENCEV_O:
        INC DI
        
        NEXT_CHECK_DEFENCEV_O:                    
        
        ADD SI,3
                             
        LOOP VERTICAL_CHECK_DEFENCEV_O
        
        POP CX
        
        XOR SI,SI
        
        INC BX
        
        ADD SI,BX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP TOTAL_VERTICAL_DEFENCEV_O
;----------------------------------------------
        XOR DI,DI
        XOR BX,BX
        MOV BX,4
        MOV CX,2
        XOR SI,SI
        
        DIAGONAL_CHECK_ATTACK_O_MAIN:
        
        PUSH CX
        
        MOV CX,3
        
        DIAGONAL_CHECK_ATTACK_O:
                             
        CMP BUFFER2[SI],1
        JE INC_COUNT_ATTACK_O_DIAGONAL 
        
        CMP BUFFER[SI],1
        JE  COUNT5_ATTACK_O_DIAGONAL
        
        MOV AX,SI
         
        JMP NEXT_CHECK_ATTACK_O_DIAGONAL
        
        COUNT5_ATTACK_O_DIAGONAL:
        MOV DI,5
        
        JMP NEXT_CHECK_ATTACK_O_DIAGONAL
        
        INC_COUNT_ATTACK_O_DIAGONAL:
        INC DI
        
        NEXT_CHECK_ATTACK_O_DIAGONAL:                    
        
        ADD SI,BX
                           
        LOOP DIAGONAL_CHECK_ATTACK_O
        
        POP CX
        
        XOR SI,SI
        XOR BX,BX      
        MOV BX,2
        MOV SI,BX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP DIAGONAL_CHECK_ATTACK_O_MAIN 
        
;----------------------------------------------
        XOR DI,DI
        XOR BX,BX
        MOV BX,4
        MOV CX,2
        XOR SI,SI
        
        DIAGONAL_CHECK_DEFENCE_O_MAIN:
        
        PUSH CX
        
        MOV CX,3
        
        DIAGONAL_CHECK_DEFENCE_O:
                             
        CMP BUFFER[SI],1
        JE INC_COUNT_DEFENCE_O_DIAGONAL 
        
        CMP BUFFER2[SI],1
        JE  COUNT5_DEFENCE_O_DIAGONAL
        
        MOV AX,SI
         
        JMP NEXT_CHECK_DEFENCE_O_DIAGONAL
        
        COUNT5_DEFENCE_O_DIAGONAL:
        MOV DI,5
        
        JMP NEXT_CHECK_DEFENCE_O_DIAGONAL
        
        INC_COUNT_DEFENCE_O_DIAGONAL:
        INC DI
        
        NEXT_CHECK_DEFENCE_O_DIAGONAL:                    
        
        ADD SI,BX
                           
        LOOP DIAGONAL_CHECK_DEFENCE_O
        
        POP CX
        
        XOR SI,SI
        XOR BX,BX      
        MOV BX,2
        MOV SI,BX
        
        CMP DI,2
        JE FINISH
        
        XOR DI,DI      
              
        LOOP DIAGONAL_CHECK_DEFENCE_O_MAIN
        
;----------------------------------------------
        
        CMP DRAW,7
        JNAE FINISH
        MOV DI,2
                
        FINISH:
        
        POP BX
        POP SI
        POP CX
        
        RET
        
SMARTCHOICE ENDP
;---------------------------------------------

RESET PROC  ;Epanafora twn parakatw timwn
    
        XOR AX,AX
        XOR BX,BX
        XOR DX,DX
        XOR SI,SI
        MOV CX,9
        
        RESET_BUFF:
        
        MOV BUFFER[SI],0
        MOV BUFFER2[SI],0
        
        INC SI
        
        LOOP RESET_BUFF
        
        MOV PLAYER,1
        MOV DRAW,0
        MOV COMMAND,0
        MOV COMMAND2,0   
        MOV TIMES2,0
        
        XOR CX,CX
        XOR SI,SI
        XOR DI,DI
        

        RET
    
RESET ENDP

;---------------------------------------------

SCREEN1 PROC ;Synarthsh gia tin klisi olon ton synarthseon tis protis othonis
        
        CALL TAF 
        CALL IOT 
        CALL EPSILON
        CALL ALPHA1
        CALL OMICRON
        CALL CI
        CALL PAULA
        CALL PLAYER33
        CALL KEY
        
        RET
        
SCREEN1 ENDP
;---------------------------------------------

TAF PROC ;Synarthsh gia ti dhmeioyrgeia tou T sto TIC TAC TOE
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        
        MOV CX,10 ;Vroxos gia ti dhmeioyrgeia ton kaheton
        MOV DX,25
        MOV BH,0
        
        TAF_LOOP1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,113
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,113
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        POP CX
        
        INC CX
        CMP CX,35
        JBE TAF_LOOP1
        
        MOV CX,23 ;Vroxos gia ti dhmeioyrgeia ton orizontion
        mov DX,25
        
        TAF_LOOP2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,113
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,113
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        POP CX
        
        INC DX
        CMP DX,60
        JBE TAF_LOOP2
        
        POP DX
        POP CX
        POP BX
        POP AX
        RET
        
TAF ENDP 
;---------------------------------------------
IOT PROC ;Synarthsh gia ti dhmeioyrgeia tou I sto TIC TAC TOE
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
             
        MOV CX,45 ;Vroxos gia ti dhmeioyrgeia tis kahetou
        MOV DX,25
        MOV BH,0 
        
        IOT_LOOP1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH DX
        ADD DX,36
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP DX
                  
        INC CX ;Vroxos gia tin dhmeioyrgeia ton pano kai kato orizontion
        CMP CX,60
        JBE iot_loop1 
        
        MOV CX,53
        MOV DX,25
        
        IOT_LOOP2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h  
        
        INC DX
        CMP DX,60
        JBE IOT_LOOP2
        
        POP DX
        POP CX
        POP BX
        POP AX          
        RET
        
IOT ENDP
;---------------------------------------------
EPSILON PROC ;Synarthsh gia ti dhmeioyrgeia tou E sto TIC TAC TOE
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX    
            
        MOV CX,290 ;Vroxos gia ti dhmeioyrgeia tis kahetou
        MOV DX,25
        MOV BH,0
        
        EPSILON_LOOP1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH DX
        ADD DX,18
        MOV AH,0Ch
        MOV AL,9
        INT 10h  
         
        PUSH DX
        ADD DX,18
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP DX
        POP DX
                 
        INC CX
        CMP CX,310
        JBE epsilon_loop1 
        
        MOV CX,290 ;Vroxos gia ti dhmeioyrgeia to trion orizontion
        MOV DX,25
        MOV BH,0
        
        EPSILON_LOOP2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h  
        
        INC DX
        CMP DX,60
        JBE EPSILON_LOOP2
        
        POP DX
        POP CX
        POP BX
        POP AX
        RET
    
EPSILON ENDP
;---------------------------------------------
ALPHA1 PROC ;Synarthsh gia ti dhmeioyrgeia tou A sto TIC TAC TOE
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX   
            
        MOV CX,152 ;Vroxos gia ti dhmeioyrgeia ton katheton
        MOV DX,27
        MOV BH,0
        
        ALPHA1_LOOP1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,21
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX
        CMP DX,60
        JBE ALPHA1_LOOP1
        
        MOV DX,25 ;Vroxoi gia tin dhmeioyrgeia ton pano kai kato orizontion
        MOV CX,154
        
        ALPHA1_LOOP2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        INC CX
        
        CMP CX,171
        JBE ALPHA1_LOOP2
        
        MOV DX,47
        MOV CX,152
          
        ALPHA1_LOOP3:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        INC CX
        
        CMP CX,173
        JBE ALPHA1_LOOP3
        
        POP DX
        POP CX
        POP BX
        POP AX
        RET

ALPHA1 ENDP
;---------------------------------------------
OMICRON PROC ;Synarthsh gia ti dhmeioyrgeia tou O sto TIC TAC TOE
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX    
            
        MOV CX,263 ;Vroxos gia ti dhmeioyrgeia ton aristera kai deksia katheton
        MOV DX,27
        MOV BH,0
        
        OMICRON_LOOP1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,22
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX
        CMP DX,58
        JBE OMICRON_LOOP1
        
        MOV CX,265 ;Vroxos gia tin dhmeioyrgeia ton pano kai kato orizontion
        MOV DX,25
        MOV BH,0
        
        OMICRON_LOOP2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH DX
        ADD DX,35
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP DX
        
        INC CX
        CMP CX,283
        JBE OMICRON_LOOP2
            
        POP DX
        POP CX
        POP BX
        POP AX
        RET
    
OMICRON ENDP
;---------------------------------------------
CI PROC ;Synarthsh gia ti dhmeioyrgeia tou C sto TIC TAC TOE
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX    
            
        MOV CX,70 ;Vroxos gia ti dhmeioyrgeia ton katheton
        MOV DX,27
        MOV BH,0
        
        CI_LOOP1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h        
        
        PUSH CX
        ADD CX,112
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX
        CMP DX,58
        JBE CI_LOOP1 
        
        MOV CX,72 ;Vroxoi gia tin dhmeioyrgeia ton pano kai kato orizontion
        MOV DX,25
        MOV BH,0
        
        CI_LOOP2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h

        PUSH DX
        ADD DX,35
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP DX
        
        INC CX
        CMP CX,93
        JBE CI_LOOP2 
        
        MOV CX,184
        MOV DX,25
        MOV BH,0
        
        CI_LOOP3:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH DX
        ADD DX,35
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP DX
        
        INC CX
        CMP CX,205
        JBE CI_LOOP3 
        
        POP DX
        POP CX
        POP BX
        POP AX
        RET

CI ENDP
;---------------------------------------------
PAULA PROC ;Synarthsi gia tin emfanisi tis paulas anamesa sto TIC TAC TOE
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
            
        MOV CX,103
        MOV DX,43
        MOV BH,0
        
        PAULA_LOOP:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,113
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC CX
        CMP CX,117
        JBE PAULA_LOOP     
        
        POP DX
        POP CX
        POP BX
        POP AX    
        RET

PAULA ENDP
;---------------------------------------------
PLAYER33 PROC ;Synarthsh gia tin emfanisi tou mhnymatos 1 player 2 players kathos kai ton plaision tous
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI
        
        MOV CX,72 ;Dhmeioyrgeia emfolevmenou vroxou gia tin kataskeyh ton orthogonion plaiseion
        MOV DX,115
        
        BEGIN2:
        
        BEGIN1:
        
        MOV AH,0Ch
        MOV BH,0
        MOV AL,3
        INT 10h
        
        PUSH CX
        ADD CX,95
        
        MOV AH,0Ch
        MOV BH,0
        MOV AL,3
        INT 10h
        POP CX
        
        INC CX
        CMP CX,145
        JB BEGIN1
        
        MOV CX,72
        INC DX
        CMP DX,133
        JB BEGIN2  
        
        ;...........
        
        XOR SI,SI ;Vroxos gia tin emfanisi tou mhnymatos
        MOV DL,9
        MOV DH,15
        
        START1:
        MOV AH,2h
        MOV BH,0
        
        INT 10h
        
        MOV AH,0Ah
        MOV BH,0
        MOV AL,EPILOGI1[si]
        MOV BL,0A5h
        MOV CX,1
        INT 10h
        
        INC DL        
        INC SI        
        CMP SI,21
        JB START1
        
        POP SI
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET

PLAYER33 ENDP
;---------------------------------------------
KEY PROC ;Synarthsh gia tin emfanisi tou press key...
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI
        
        MOV SI,0 ;Vroxos emfanisis tou mhnymatos
        MOV DL,11
        MOV DH,21
        
        KEY1:
        MOV AH,2h
        MOV BH,0
        
        INT 10h
        
        MOV AH,0Ah
        MOV BH,0
        MOV AL,MESSAGE1[si]
        MOV BL,4
        MOV CX,1
        INT 10h
        
        INC DL        
        INC SI        
        CMP SI,16
        JB KEY1

        POP SI
        POP DX
        POP CX
        POP BX
        POP AX 
        
        RET
        
KEY ENDP
;---------------------------------------------
KEY2 PROC ;Synarthsh gia tin emfanisi tou press key...
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI
        
        MOV SI,0 ;Vroxos emfanisis tou mhnymatos
        MOV DL,11
        MOV DH,21
        
        KEY12:
        MOV AH,2h
        MOV BH,0
        
        INT 10h
        
        MOV AH,0Ah
        MOV BH,0
        MOV AL,MESSAGE7[si]
        MOV BL,4
        MOV CX,1
        INT 10h
        
        INC DL        
        INC SI        
        CMP SI,16
        JB KEY12

        POP SI
        POP DX
        POP CX
        POP BX
        POP AX 
        
        RET
        
KEY2 ENDP
;---------------------------------------------

SCREEN2 PROC ;Synarthsh gia tin klisi olon ton synarthseon tis deyteris othonis

        CALL TAF 
        CALL IOT 
        CALL EPSILON
        CALL ALPHA1
        CALL OMICRON
        CALL CI
        CALL PAULA
        CALL TURN 
        CALL KEY
        CALL BOX

        RET
        
SCREEN2 ENDP
;---------------------------------------------
TURN PROC ;Synarthsh gia tin emfanisi tou choose your turn
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        MOV SI,0 ;Vroxos emfanisis tou mhnymatos
        MOV DL,10
        MOV DH,12
          
        TURN1:
        MOV AH,2h
        MOV BH,0
        
        INT 10h
        
        MOV AH,0Ah
        MOV BH,0
        MOV AL,MESSAGE2[si]
        MOV BL,7
        MOV CX,1
        INT 10h
        
        INC DL        
        INC SI        
        CMP SI,19
        JB TURN1

        POP SI
        POP DX
        POP CX
        POP BX
        POP AX 
        
        RET
        
TURN ENDP
;---------------------------------------------
BOX PROC ;Synarthsh gia tin emfanisi tou mhnymatos player 1 player 2 kai ton plaision tous
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI

        MOV SI,0 ;Vroxos emfanisis tou mhnymatos
        MOV DL,12
        MOV DH,16
      
        BOX1:
        MOV AH,2h
        MOV BH,0
        
        INT 10h
        
        MOV AH,0Ah
        MOV BH,0
        MOV AL,EPILOGI2[si]
        MOV BL,0EH
        MOV CX,1
        INT 10h
        
        INC DL        
        INC SI        
        CMP SI,14
        JB BOX1
        
        MOV CX,89 ;Vroxoi gia ti dhmeiourgeia ton orizontion ton orthoginion 
        MOV DX,124
        MOV BH,0
        
        BOX2:
        MOV AH,0Ch
        MOV AL,3
        INT 10h
        
        PUSH CX
        ADD CX,69
        MOV AH,0Ch
        MOV AL,3
        INT 10h
        POP CX
        
        INC CX
        CMP CX,140
        JBE BOX2   
        
        MOV CX,89
        MOV DX,138
        MOV BH,0
        
        BOX3:
        MOV AH,0Ch
        MOV AL,3
        INT 10h
        
        PUSH CX
        ADD CX,69
        MOV AH,0Ch
        MOV AL,3
        INT 10h
        POP CX
        
        INC CX
        CMP CX,140
        JBE BOX3 
        
        MOV CX,89 ;Vroxoi gia ti dhmeiourgeia ton katheton ton orthoginion
        MOV DX,124
        MOV BH,0
        
        BOX4:
        MOV AH,0Ch
        MOV AL,3
        INT 10h
        
        PUSH CX
        ADD CX,51
        MOV AH,0Ch
        MOV AL,3
        INT 10h
        POP CX
        
        INC DX
        CMP DX,137
        JBE BOX4 
        
        MOV CX,158
        MOV DX,124
        MOV BH,0    
        
        BOX5:
        MOV AH,0Ch
        MOV AL,3
        INT 10h
        
        PUSH CX
        ADD CX,51
        MOV AH,0Ch
        MOV AL,3
        INT 10h
        POP CX
        
        INC DX
        CMP DX,137
        JBE BOX5 


        POP SI
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET
        
BOX ENDP
;---------------------------------------------
SCREEN3 PROC ;Synarthsh gia tin klisi olon ton synarthseon tis tritis othonis

        CALL TAF 
        CALL IOT 
        CALL EPSILON
        CALL ALPHA1
        CALL OMICRON
        CALL CI
        CALL PAULA
        
        CMP HOW_MANY,2
        JNE ONE_NAME
         
        CALL NAME_MSG
        CALL PLAYER1_MSG
        
        CALL GRAPH
        
        CALL TAF 
        CALL IOT 
        CALL EPSILON
        CALL ALPHA1
        CALL OMICRON
        CALL CI
        CALL PAULA
        
        CALL NAME_MSG
        CALL PLAYER2_MSG
        
        JMP THIS_IS_THE_END
        
        ONE_NAME:
        
        CMP TEMP,1
        JNE SECOND_NAME
        
        CALL NAME_MSG
        CALL PLAYER1_MSG
        
        MOV TEXT4[3],"P"
        MOV TEXT4[4],"C"
        MOV TEXT4[5]," "
        
        JMP THIS_IS_THE_END
        
        SECOND_NAME:
        
        CALL NAME_MSG
        CALL PLAYER2_MSG
        
        MOV TEXT2[3],"P"
        MOV TEXT2[4],"C"
        MOV TEXT2[5]," "
        
        THIS_IS_THE_END:

        RET
        
SCREEN3 ENDP


;---------------------------------------------  
NAME_MSG PROC ;Synarthsh gia tin emfanisi tou type your...

         PUSH AX
         PUSH BX
         PUSH CX
         PUSH DX
         PUSH SI
       
         MOV DL,10
         MOV DH,12
         CALL CURSOR_POSITION
         
         ;....................................
         
         MOV CX,17
         XOR SI,SI
         
         NAME_MSG1:
         
         MOV AL,MESSAGE3[SI]
         MOV BL,7
         CALL CHAR_OUTPUT
         
         INC DL
         CALL CURSOR_POSITION        
         INC SI        
       
         LOOP NAME_MSG1
         
         ;....................................
         
         POP SI
         POP DX
         POP CX
         POP BX
         POP AX
         RET
    
NAME_MSG ENDP
;---------------------------------------------
PLAYER1_MSG PROC ;Synarthsh gia tin emfanisi tou onomatos tou protou paikti

         PUSH AX
         PUSH BX
         PUSH CX
         PUSH DX
         PUSH SI
      
         MOV DL,10
         MOV DH,16
         CALL CURSOR_POSITION
         
         ;....................................
         
         MOV CX,10
         XOR SI,SI
         PLAYER_MSG1: 
         
         MOV AL,MESSAGE4[SI]
         MOV BL,5
         CALL CHAR_OUTPUT
         
         INC DL
         CALL CURSOR_POSITION        
         INC SI        

         LOOP PLAYER_MSG1
         
         ;....................................
         
         MOV DL,7
         MOV DH,22
         CALL CURSOR_POSITION
         INC DL
         
         ;....................................
         
         MOV CX,23
         XOR SI,SI
         ENTER:
         MOV AL,ENTERMSG[SI]
         MOV BL,4H
         CALL CHAR_OUTPUT
         CALL CURSOR_POSITION
         INC DL
         INC SI
         LOOP ENTER
         
         ;....................................
         
         MOV DL,19
         MOV DH,16
         CALL CURSOR_POSITION
         
         ;....................................
         
         MOV SI,3
         GIVENAME:
         MOV AH,8H
         INT 21H
         CMP AL,13
         JE ENDNAME
         CMP AL,8
         JNE WRITE
         
         CMP SI,3
         JE GIVENAME
         
         CALL CURSOR_POSITION
         
         MOV AL,"."
         DEC SI
         
         MOV TEXT2[SI],AL
         MOV BL,08H
         MOV AL," "
         CALL CHAR_OUTPUT
         DEC DL
         JMP GIVENAME
         
         WRITE:
         CMP SI,18
         JA GIVENAME
         
         INC DL
         CALL CURSOR_POSITION
         MOV TEXT2[SI],AL
         MOV BL,8H
         CALL CHAR_OUTPUT
         
         INC SI
         
         JMP GIVENAME
         
         ;....................................
         
         ENDNAME:
         MOV TEXT2[SI]," "
          
         POP SI
         POP DX
         POP CX
         POP BX
         POP AX
         RET
    
PLAYER1_MSG ENDP

;---------------------------------------------
PLAYER2_MSG PROC ;Synarthsh gia tin emfanisi tou onomatos tou deyterou paikti

         PUSH AX
         PUSH BX
         PUSH CX
         PUSH DX
         PUSH SI
       
         MOV DL,10
         MOV DH,16
         CALL CURSOR_POSITION
         
         ;....................................
         
         MOV CX,10
         XOR SI,SI
         
         PLAYER_MSG2: 
         
         MOV AL,MESSAGE5[SI]
         MOV BL,5
         CALL CHAR_OUTPUT
         
         INC DL
         CALL CURSOR_POSITION        
         INC SI        

         LOOP PLAYER_MSG2 
         
         ;....................................
         
         MOV DL,7
         MOV DH,22
         CALL CURSOR_POSITION
         INC DL
         
         ;....................................
         
         MOV CX,23
         XOR SI,SI
         ENTER2:
         MOV AL,ENTERMSG[SI]
         MOV BL,4H
         CALL CHAR_OUTPUT
         CALL CURSOR_POSITION
         INC DL
         INC SI
         LOOP ENTER2
         
         ;....................................
         
         MOV DL,19
         MOV DH,16
         CALL CURSOR_POSITION
         
         ;....................................
         
         MOV SI,3
         GIVENAME2:
         MOV AH,8H
         INT 21H
         CMP AL,13
         JE ENDNAME2          
         CMP AL,8
         JNE WRITE2
         
         CMP SI,3
         JE GIVENAME2
         
         CALL CURSOR_POSITION
         
         MOV AL,"."
         DEC SI
         MOV TEXT4[SI],AL
         MOV BL,08H
         MOV AL," "
         CALL CHAR_OUTPUT
         DEC DL
         JMP GIVENAME2
         
         WRITE2:
         
         CMP SI,18
         JA GIVENAME2
         
         INC DL
         CALL CURSOR_POSITION
         MOV TEXT4[SI],AL
         MOV BL,08H
         CALL CHAR_OUTPUT
         
         INC SI
         
         JMP GIVENAME2
         
         ;....................................
         
         ENDNAME2:
         MOV TEXT4[SI]," " 
          
         POP SI
         POP DX
         POP CX
         POP BX
         POP AX
         RET
    
PLAYER2_MSG ENDP
;--------------------------------------------- 

ALPHA2 PROC ;Synarthsh gia th dhmeiourgeia tou A gia to hall of fame
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX    
            
        MOV CX,36
        MOV DX,27
        MOV BH,0
        
        alpha2_loop1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,21
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        PUSH CX
        ADD CX,197
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        PUSH CX
        ADD CX,218
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX
        CMP DX,60
        JBE alpha2_loop1
        
        MOV DX,25
        MOV CX,38
        alpha2_loop2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,197
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC CX
        CMP CX,55
        JBE alpha2_loop2
        
        MOV DX,47
        MOV CX,36
        alpha2_loop3:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,197
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC CX
        CMP CX,57
        JBE alpha2_loop3
        
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET

ALPHA2 ENDP 
;----------------------------------------
HTA PROC ;Synarthsh gia th dhmeiourgeia tou H gia to hall of fame
       
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX  
            
        MOV CX,10 ;Vroxos gia ti dhmeioyrgeia ton katheton
        MOV DX,25
        MOV BH,0
        
        hta_loop1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
            
        PUSH CX
        ADD CX,22
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX
        CMP DX,60
        JBE hta_loop1
        
        MOV DX,44 ;Vroxos gia ti dhmeioyrgeia ton orizontion
        MOV CX,10
        
        hta_loop2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        INC CX
        
        CMP CX,32
        JBE hta_loop2
        
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET

HTA ENDP    
;----------------------------------------
LAMDA PROC ;Synarthsh gia th dhmeiourgeia tou L gia to hall of fame
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX   
            
        MOV CX,63 ;Vroxos gia th dhmeioyrgeia ton katheton
        MOV DX,25
        MOV BH,0
        
        lamda_loop1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,22
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX
        CMP DX,60
        JBE lamda_loop1
        
        MOV DX,60 ;Vroxos gia th dhmeioyrgeia ton orizontion
        MOV CX,63
        
        lamda_loop2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,23
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC CX
        CMP CX,79        
        JBE lamda_loop2
        
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET

LAMDA ENDP
;----------------------------------------
OMICRON2 PROC ;Synarthsh gia th dhmeiourgeia tou O gia to hall of fame
    
        
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX  
            
        MOV CX,136
        MOV DX,27
        MOV BH,0
        
        omicron2_loop1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,22
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX
        CMP DX,58
        JBE omicron2_loop1
        
        MOV CX,138
        MOV DX,25
        MOV BH,0
        
        omicron2_loop2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH DX
        ADD DX,35
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP DX
        
        INC CX
        CMP CX,156
        JBE omicron2_loop2
            
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET
    
OMICRON2 ENDP 
;----------------------------------------
FI PROC ;Synarthsh gia th dhmeiourgeia tou F gia to hall of fame
       
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX    
            
        MOV CX,162 ;Vroxos gia ti dhmeioyrgeia ton katheton
        MOV DX,25
        MOV BH,0
        
        fi_loop1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,47
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX ;Vroxoi gia tin dhmeioyrgeia ton dyo orizontion 
        CMP DX,60
        JBE fi_loop1
        
        MOV DX,25
        MOV CX,162
        fi_loop2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,47
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC CX
        CMP CX,181
        JBE fi_loop2
        
        MOV DX,44
        MOV CX,162
        fi_loop3:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,47
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC CX
        CMP CX,172
        JBE fi_loop3
        
        
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET
        
FI ENDP   
;----------------------------------------
MI PROC ;Synarthsh gia th dhmeiourgeia tou M gia to hall of fame
        
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX  
            
        MOV CX,258 ;Vroxos gia ti dimeioyrgeia ton dyo katheton
        MOV DX,25
        MOV BH,0
        
        mi_loop1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH CX
        ADD CX,22
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP CX
        
        INC DX
        CMP DX,60
        JBE mi_loop1
        
        MOV CX,258
        MOV DX,25
        MOV BH,0
        
        mi_loop2: ;Oi dyo vroxoi gia ti dhmeioyrgeia ton diagonion
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        INC DX
        INC CX
        CMP CX,269
        JBE mi_loop2
        
        MOV CX,270
        MOV DX,36
        MOV BH,0
        
        mi_loop3:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        DEC DX
        INC CX
        CMP CX,279
        JBE mi_loop3
        
        
        POP DX
        POP CX
        POP BX
        POP AX
       
        RET
        
MI ENDP 
;----------------------------------------
EPSILON2 PROC ;Synarthsh gia th dhmeiourgeia tou E gia to hall of fame
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX   
            
        MOV CX,285
        MOV DX,25
        MOV BH,0
        
        epsilon2_loop1:
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        
        PUSH DX
        ADD DX,18
        MOV AH,0Ch
        MOV AL,9
        INT 10h  
        
        PUSH DX
        ADD DX,18
        MOV AH,0Ch
        MOV AL,9
        INT 10h
        POP DX
        POP DX
                  
        INC CX
        CMP CX,305
        JBE epsilon2_loop1 
        
        MOV CX,285
        MOV DX,25
        MOV BH,0
        
        epsilon2_loop2:
        MOV AH,0Ch
        MOV AL,9
        INT 10h  
        
        INC DX
        CMP DX,60
        JBE epsilon2_loop2
        
        POP DX
        POP CX
        POP BX
        POP AX
        
        RET
    
EPSILON2 ENDP 
;----------------------------------------
KEYGAN PROC ;Synarthsh gia tin emfanish toy mhnymatos prees any key...
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI
        
        MOV SI,0 ;Vroxos gia tin emfanisi toy mhnymatos
        MOV DL,7
        MOV DH,22
        
        keygan1:
        MOV AH,2h
        MOV BH,0
        INT 10h
        
        MOV AH,0Ah
        MOV BH,0
        MOV AL,MESSAGE6[SI]
        MOV BL,4
        MOV CX,1
        INT 10h
        
        INC DL        
        INC SI        
        CMP SI,25
        Jb keygan1
        
        MOV AH,8H
        INT 21H
        
        POP SI
        POP DX
        POP CX
        POP BX
        POP AX 
        
        RET
        
KEYGAN ENDP
;----------------------------------------
NAMES PROC ;Synarthsh gia tin emfanisi tis listas ton onomaton kai ton nikon
    
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH SI
        
        XOR SI,SI ;Broxos gia tin emfanish tou mhnymatos
        MOV DL,7
        MOV DH,10
        CALL CURSOR_POSITION
        INC DL
        ;.........................
        MOV CX,25
        LISTING:
        MOV AL,WINSMSG[SI]
        MOV BL,9H
        CALL CHAR_OUTPUT
        CALL CURSOR_POSITION
        INC DL
        INC SI
        LOOP LISTING
        ;.........................
        XOR SI,SI ;Broxos gia tin emfanish tou mhnymatos
        MOV DL,4
        MOV DH,12
        CALL CURSOR_POSITION
        INC DL
        ;.........................
        MOV CX,24
        LISTING2:
        MOV AL,TEXT2[SI]
        MOV BL,0AH
        CALL CHAR_OUTPUT
        CALL CURSOR_POSITION
        INC DL
        INC SI
        LOOP LISTING2
        ;.........................
        MOV CX,3
        XOR SI,SI
        DEC DL
        PRINTVALUE1:
        CALL CURSOR_POSITION
        INC DL
        MOV AL,TEXT3[SI]
        MOV BL,0AH
        CALL CHAR_OUTPUT
        INC SI
        LOOP PRINTVALUE1
        ;.........................
        XOR SI,SI ;Broxos gia tin emfanish tou mhnymatos
        MOV DL,4
        MOV DH,14
        CALL CURSOR_POSITION
        INC DL
        ;.........................
        MOV CX,24
        LISTING3:
        MOV AL,TEXT4[SI]
        MOV BL,0AH
        CALL CHAR_OUTPUT
        CALL CURSOR_POSITION
        INC DL
        INC SI
        LOOP LISTING3
        ;.........................
        MOV CX,3
        XOR SI,SI
        DEC DL
        PRINTVALUE2:
        CALL CURSOR_POSITION
        INC DL
        MOV AL,TEXT5[SI]
        MOV BL,0AH
        CALL CHAR_OUTPUT
        INC SI
        LOOP PRINTVALUE2
        
        POP SI
        POP DX
        POP CX
        POP BX
        POP AX     
    
        RET
      
NAMES ENDP 
;----------------------------------------
HALL_OF_FAME PROC ;Synarthsh pou kalei oles tis ypoloipes synarhseis gia to sxhmatismo ths othonis tou hall of fame
    
        CALL ALPHA2 
        CALL HTA
        CALL LAMDA
        CALL OMICRON2
        CALL FI
        CALL MI
        CALL EPSILON2
        CALL NAMES 
        CALL KEYGAN
         
        RET
        
HALL_OF_FAME ENDP 
;---------------------------------------------
KODIKAS ENDS
SOROS SEGMENT STACK
    
    DB 256 dup(0)
    
SOROS ENDS
END ARXH