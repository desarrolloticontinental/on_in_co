DISABLE TRIGGERS FOR LOAD OF integral.factabla.
DEF VAR s-tabla AS CHAR INIT 'DVXDSF'.

FOR EACH integral.factabla WHERE integral.factabla.codcia = 1 
    AND integral.factabla.tabla = s-tabla:
    DELETE integral.factabla.
END.
FOR EACH lima.factabla NO-LOCK WHERE lima.factabla.codcia = 1
    AND lima.factabla.tabla = 'DVXDSF':
    BUFFER-COPY lima.factabla TO integral.factabla.
END.
MESSAGE 'todo ok'.
