
FOR EACH almstkal EXCLUSIVE-LOCK WHERE almstkal.codcia = 1
    AND almstkal.codmat = '099268':
    DELETE almstkal.
END.

FOR EACH almstkge EXCLUSIVE-LOCK WHERE almstkge.codcia = 1
    AND almstkge.codmat = '099268':
    DELETE almstkge.
END.

FOR EACH almmmate EXCLUSIVE-LOCK WHERE almmmate.codcia = 1
    AND almmmate.codmat = '099268':
    almmmate.stkact = 0.
END.

FOR EACH almdmov EXCLUSIVE-LOCK WHERE almdmov.codcia = 1
    AND almdmov.codmat = '099268':
    DELETE almdmov.
END.
