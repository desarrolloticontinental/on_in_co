DECLARE tt-tempo CURSOR FOR SELECT codmat, desmat FROM almmmatg WHERE almmmatg.codcia = 1 AND 
    INDEX(almmmatg.desmat, '50133') > 0 AND  INDEX(almmmatg.desmat, 'rojo') > 0.

DEF VAR X1 AS CHAR.
DEF VAR X2 AS CHAR.

OPEN tt-tempo .
FETCH tt-tempo INTO x1, x2.
REPEAT:
    DISPLAY x1 x2 FORMAT 'x(60)' WITH STREAM-IO NO-BOX WIDTH 320.
    FETCH tt-tempo INTO x1, x2.
END.





