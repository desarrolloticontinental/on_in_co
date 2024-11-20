OUTPUT TO c:\tmp\almmmatg.d.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1:
    EXPORT almmmatg.
END.
OUTPUT TO c:\tmp\almmmate.d.
FOR EACH almmmate NO-LOCK WHERE codcia = 1
    AND LOOKUP(codalm, '11,10a,10,27,501,502,503') > 0:
    EXPORT almmmate.
END.
