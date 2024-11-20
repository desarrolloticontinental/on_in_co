DEF VAR X AS CHAR.

INPUT FROM d:\eliam.prn.
REPEAT :
    IMPORT UNFORMATTED X.
    IF TRUE <> (X > '') THEN LEAVE.
    FIND almmmatg WHERE codcia = 1
        AND codmat = SUBSTRING(X,1,6)
        NO-ERROR.
    IF codmat = '079812' THEN ASSIGN monvta = 2 DsctoProm[1] = 2.
    CtoTot = DECIMAL(SUBSTRING(X,11)).
    DsctoProm[2] = DECIMAL(SUBSTRING(X,11)).
    CtoLis = DECIMAL(SUBSTRING(X,11)) / 1.18.
END.
