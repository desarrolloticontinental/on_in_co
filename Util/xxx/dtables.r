	��VT�jR,7   �                                              �' 372C00F0utf-8 MAIN C:\newsie\on_in_co\Util\xxx\dtables.w,, PROCEDURE disable_UI,, PROCEDURE bufferCommit,,OUTPUT pocMessages CHARACTER,OUTPUT pocUndoIds CHARACTER PROCEDURE serverCommit,,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE remoteCommit,,INPUT-OUTPUT pcContext CHARACTER,INPUT-OUTPUT RowObjUpd TABLE,OUTPUT pcMessages CHARACTER,OUTPUT pcUndoIds CHARACTER PROCEDURE pushTableAndValidate,,INPUT pcValType CHARACTER,INPUT-OUTPUT RowObjUpd TABLE PROCEDURE pushRowObjUpdTable,,INPUT RowObjUpd TABLE PROCEDURE initProps,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE viewObject,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE disconnectObject,, PROCEDURE destroyObject,, PROCEDURE bindServer,, PROCEDURE transferDBRow,,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER PROCEDURE startFilter,, PROCEDURE releaseDBRow,, PROCEDURE refetchDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE filterContainerHandler,,INPUT phFilterContainer HANDLE PROCEDURE fetchDBRowForUpdate,, PROCEDURE confirmContinue,,INPUT-OUTPUT pioCancel LOGICAL PROCEDURE compareDBRow,, PROCEDURE bufferCopyDBToRO,,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER PROCEDURE assignDBRow,,INPUT phRowObjUpd HANDLE PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateQueryPosition,, PROCEDURE updateAddQueryWhere,,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER PROCEDURE undoTransaction,, PROCEDURE transferToExcel,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE synchronizeProperties,,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER PROCEDURE submitValidation,,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER PROCEDURE submitForeignKey,,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER PROCEDURE submitCommit,,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL PROCEDURE startServerObject,, PROCEDURE setPropertyList,,INPUT pcProperties CHARACTER PROCEDURE serverFetchRowObjUpdTable,,OUTPUT phRowObjUpd TABLE-HANDLE PROCEDURE serverSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE PROCEDURE saveContextAndDestroy,,OUTPUT pcContext CHARACTER PROCEDURE rowObjectState,,INPUT pcState CHARACTER PROCEDURE retrieveFilter,, PROCEDURE restartServerObject,, PROCEDURE remoteSendRows,,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT phRowObject TABLE-HANDLE,OUTPUT pocMessages CHARACTER PROCEDURE refreshRow,, PROCEDURE printToCrystal,,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER PROCEDURE isUpdatePending,,INPUT-OUTPUT plUpdate LOGICAL PROCEDURE initializeServerObject,, PROCEDURE initializeObject,, PROCEDURE home,, PROCEDURE genContextList,,OUTPUT pcContext CHARACTER PROCEDURE fetchPrev,, PROCEDURE fetchNext,, PROCEDURE fetchLast,, PROCEDURE fetchFirst,, PROCEDURE fetchBatch,,INPUT plForwards LOGICAL PROCEDURE endClientDataRequest,, PROCEDURE destroyServerObject,, PROCEDURE describeSchema,,INPUT pcSdoName CHARACTER,OUTPUT hTtSchema TABLE-HANDLE PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE copyColumns,,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE PROCEDURE commitTransaction,, PROCEDURE clientSendRows,,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER PROCEDURE batchServices,,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER FUNCTION deleteRecordStatic,logical,INPUT piTableIndex INTEGER FUNCTION getRowObjUpdStatic,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION whereClauseBuffer,CHARACTER,INPUT pcWhere CHARACTER FUNCTION setQueryWhere,LOGICAL,INPUT pcWhere CHARACTER FUNCTION setQueryString,LOGICAL,INPUT pcQueryString CHARACTER FUNCTION setQuerySort,LOGICAL,INPUT pcSort CHARACTER FUNCTION setQueryPosition,LOGICAL,INPUT pcPosition CHARACTER FUNCTION rowidWhereCols,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION rowidWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION resolveBuffer,CHARACTER,INPUT pcBuffer CHARACTER FUNCTION removeQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER FUNCTION removeForeignKey,LOGICAL, FUNCTION refreshRowident,CHARACTER,INPUT pcRowident CHARACTER FUNCTION newWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryWhere,CHARACTER,INPUT pcWhere CHARACTER FUNCTION newQueryValidate,CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION newQueryString,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER FUNCTION insertExpression,CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER FUNCTION indexInformation,CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER FUNCTION getTargetProcedure,HANDLE, FUNCTION getQueryWhere,CHARACTER, FUNCTION getQueryString,CHARACTER, FUNCTION getQuerySort,CHARACTER, FUNCTION getQueryPosition,CHARACTER, FUNCTION getForeignValues,CHARACTER, FUNCTION getDataColumns,CHARACTER, FUNCTION excludeColumns,CHARACTER,INPUT iTable INTEGER FUNCTION dbColumnHandle,HANDLE,INPUT pcColumn CHARACTER FUNCTION dbColumnDataName,CHARACTER,INPUT pcDbColumn CHARACTER FUNCTION columnValMsg,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnTable,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnQuerySelection,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDbColumn,CHARACTER,INPUT pcColumn CHARACTER FUNCTION columnDataType,CHARACTER,INPUT pcColumn CHARACTER FUNCTION bufferWhereClause,CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER FUNCTION bufferCompareDBToRO,LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER FUNCTION assignQuerySelection,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION addQueryWhere,LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER FUNCTION getObjectType,character, FUNCTION updateRow,LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER FUNCTION submitRow,LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER FUNCTION rowValues,CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION rowAvailable,LOGICAL,INPUT pcDirection CHARACTER FUNCTION prepareQuery,LOGICAL,INPUT pcQuery CHARACTER FUNCTION openQuery,LOGICAL, FUNCTION openDataQuery,LOGICAL,INPUT pcPosition CHARACTER FUNCTION hasForeignKeyChanged,LOGICAL, FUNCTION getLastCommitErrorType,CHARACTER, FUNCTION firstRowIds,CHARACTER,INPUT pcQueryString CHARACTER FUNCTION findRowWhere,LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER FUNCTION findRow,LOGICAL,INPUT pcKeyValues CHARACTER FUNCTION fetchRowIdent,CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER FUNCTION fetchRow,CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER FUNCTION deleteRow,LOGICAL,INPUT pcRowIdent CHARACTER FUNCTION createRow,LOGICAL,INPUT pcValueList CHARACTER FUNCTION copyRow,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION columnProps,CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER FUNCTION closeQuery,LOGICAL, FUNCTION canNavigate,LOGICAL, FUNCTION cancelRow,CHARACTER, FUNCTION addRow,CHARACTER,INPUT pcViewColList CHARACTER TEMP-TABLE RowObjUpd 0,RowNum:RowNum 0 NO,CodAlm character 0 0,Descripcion character 1 0,CodCia integer 2 0,RowNum integer 3 0,RowIdent character 4 0,RowMod character 5 0,RowIdentIdx character 6 0,RowUserProp character 7 0,ChangedFields character 8 0     <              �             �� <  ��              p�              �>  	   +   d� �  W   � `  X   d�   Y   x�   [   ��   \   �� <  ]   ��    ^   �� \  `   X� 0  a   ? �� %  iSO8859-1                                                                           |   ! �                                      �                   x�   	             �  �    �   /�   T�              `�  �                                                                PROGRESS                         X           
    
                    �              �                                                                                                     
  h                                                                                                                                  INTEGRAL                         PROGRESS                         �                                        u!5O               Q�                              �  �                      t  �  z      CODALMDESCRIPCIONCODCIATDOARTAUTMOVDIRALMHORRECENCALMTELALMCORRSALCORRINGCORRTRFCODDIVCLAVEALMCSGTPOCSGCODCLIFLGREPCAMPO-C                                                                        	          
                                                                                                     
          �      �  
    
                  l  4             �                                                                                          �          
  �        ,  
    
                    �             �                                                                                                    
  \  !      �  
    
                  �  �             H                                                                                          !          
    .      �  
    
                  p  8             �                                                                                          .          
  �  A      0  
    
                    �  	           �                                                                                          A          
  `  S      �  
    
                  �  �  
           L                                                                                          S          
  	  h      �  
    
                  t  <	             �                                                                                          h          
  �	  ~      4	  
    
                   	  �	             �	                                                                                          ~          
  d
  �      �	                         �	  �
             P
                                                                                          �              �      �
                        x
  @             �
                                                                                          �            �  �      8  
    
                  $  �             �                                                                                          �          
  h  �      �  
    
                  �  �             T                                                                                          �          
    �      �  
    
                  |  D                                                                                                        �          
  �  �      <                        (  �             �                                                                                          �            l  �      �                        �  �             X                                                                                          �              �      �                        �  H                                                                                                       �                �      @                        ,  �             �                                                                                          �            ,         �       �  X  H     d  �  v      �         �             �          h      �              �       �  X  �     �  �  ��      X  	       �         �    �          �      �                 ��                                               ��            h  L l�                �         
             
             
                                         
                                                                                                               
             
                                          L   \   l   |   �   �   �   �   �   �   �   �       ,  <  L  \      L   \   l   |   �   �   �   �   �   �   �   �      ,  <  L  \                                                                                                                                     	                                 �  �                             ,  8  @  X  L          \             t  |  �  �  �          �             �  �  �  �                             �  �  �  �                              �  �  �  �                                                                  $  0  8  D                                                                          CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  CodCia  999 Cia Cia 0   C�digo de Compa�ia  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���	������               �        �        �                �     i     i     i     	 	 	    t  {  �  �  �  �  �  �                                                                                                                                     	                  
                                 `  h  p  �  x          �             �  �  �  �  �          �             �  �  �  �  �          �                    (                             ,  8  @  L                              P  X  `  h                             l  x  �  �                             �  �  �  �                              �  �  �  �                                                                          CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  CodCia  999 Cia Cia 0   C�digo de Compa�ia  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     ChangedFields   x(8)    ChangedFields       �  ���
������               �        �        �                �     i     i     i     	 	 	    t  {  �  �  �  �  �  �  �    ��                            ����                            �    ��                    �l       ��                    �&    undefined                                                               �       ��  �   l   �  ��                    �����               0[�                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D       �     9          ��    �   �  4      d       4   ����d                 D                      ��                  �   �                   T]�                       �   �  �  	  �   x                                        3   ����|       O   �   ��  ��  �   batchServices                               4        ��                  R  U  L              D�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             d               ��                  �           ��                            ����                            clientSendRows                              �  p      ��                  W  ]  �              ��x                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                �               ��   <                            ��   d             0               ��                  X           ��                            ����                            commitTransaction                               X  @      ��                  _  `  p              TQ�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyColumns                             X  @      ��                  b  e  p              ��b                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               �� 
                 �  
         ��                            ����                            dataAvailable                               �  �      ��                  g  i  �              @ك                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            describeSchema                              �	  �	      ��                  k  n  �	              l�h                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <
             
               �� 
          �       0
  
         ��                            ����                            destroyServerObject                             0        ��                  p  q  H              �
�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            endClientDataRequest                                <  $      ��                  s  t  T              4�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchBatch                              <  $      ��                  v  x  T              P�h                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  l           ��                            ����                            fetchFirst                              d  L      ��                  z  {  |              H��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               d  L      ��                  }  ~  |              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               d  L      ��                  �  �  |              �b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               d  L      ��                  �  �  |              l�b                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            genContextList                              h  P      ��                  �  �  �              �b                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            home                                �  t      ��                  �  �  �              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �  |      ��                  �  �  �              4�~                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �  �      ��                  �  �  �              �~                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            isUpdatePending                             �  �      ��                  �  �  �              H��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            printToCrystal                              �  �      ��                  �  �  �              �?�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   4                             ��   \             (               ��                  P           ��                            ����                            refreshRow                              H  0      ��                  �  �  `              �V�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            remoteSendRows                              L  4      ��                  �  �  d              lW�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             |               ��   �             �               ��                 �               ��   (             �               ��   P                            ��   x             D               �� 
  �      �       l  
             ��                  �           ��                            ����                            restartServerObject                             �  |      ��                  �  �  �               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            retrieveFilter                              �  �      ��                  �  �  �              d�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            rowObjectState                              �  �      ��                  �  �  �              0�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            saveContextAndDestroy                               �  �      ��                  �  �  �              ܟ�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                               ��                            ����                            serverSendRows                              �   �       ��                  �  �  !              X�\                    O   ����    e�          O   ����    R�          O   ����    ��            ��   `!             ,!               ��   �!             T!               ��   �!             |!               ��   �!             �!               ��    "             �!               �� 
          �       �!  
         ��                            ����                            serverFetchRowObjUpdTable                               �"  �"      ��                  �  �  #              L]                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
          �       ,#  
         ��                            ����                            setPropertyList                             ($  $      ��                  �  �  @$              t��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  X$           ��                            ����                            serverSendRows                              T%  <%      ��                  �  �  l%              4��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �%             �%               ��   �%             �%               ��   &             �%               ��   0&             �%               ��   X&             $&               �� 
          �       L&  
         ��                            ����                            startServerObject                               L'  4'      ��                  �  �  d'              آp                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            submitCommit                                P(  8(      ��                  �  �  h(              ��p                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �(             �(               ��                  �(           ��                            ����                            submitForeignKey                                �)  �)      ��                  �  �  �)              �[�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   *             �)               ��   4*              *               ��                  (*           ��                            ����                            submitValidation                                (+  +      ��                  �  �  @+              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �+             X+               ��                  �+           ��                            ����                            synchronizeProperties                               �,  l,      ��                  �  �  �,              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �,             �,               ��                  �,           ��                            ����                            transferToExcel                             �-  �-      ��                    	  �-              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <.             .               ��   d.             0.               ��   �.             X.               ��                  �.           ��                            ����                            undoTransaction                             |/  d/      ��                      �/              �&�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateAddQueryWhere                             �0  l0      ��                      �0              t'�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �0             �0               ��                  �0           ��                            ����                            updateQueryPosition                             �1  �1      ��                      �1              x��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �2  �2      ��                      �2              LD�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  3           ��                            ����                            addRow          t3      �3     f       CHARACTER,INPUT pcViewColList CHARACTER cancelRow   |3      �3      �3   	 m       CHARACTER,  canNavigate �3      �3      (4    w       LOGICAL,    closeQuery  4      44      `4   
 �       LOGICAL,    columnProps @4      l4      �4    �       CHARACTER,INPUT pcColList CHARACTER,INPUT pcPropList CHARACTER  colValues   x4      �4      5   	 �       CHARACTER,INPUT pcViewColList CHARACTER copyRow �4      ,5      T5    �       CHARACTER,INPUT pcViewColList CHARACTER createRow   45      |5      �5   	 �       LOGICAL,INPUT pcValueList CHARACTER deleteRow   �5      �5      �5   	 �       LOGICAL,INPUT pcRowIdent CHARACTER  fetchRow    �5      6      H6  	  �       CHARACTER,INPUT piRow INTEGER,INPUT pcViewColList CHARACTER fetchRowIdent   (6      �6      �6  
  �       CHARACTER,INPUT pcRowIdent CHARACTER,INPUT pcViewColList CHARACTER  findRow �6      �6       7    �       LOGICAL,INPUT pcKeyValues CHARACTER findRowWhere     7      D7      t7    �       LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  firstRowIds T7      �7      �7    �       CHARACTER,INPUT pcQueryString CHARACTER getLastCommitErrorType  �7       8      X8    �       CHARACTER,  hasForeignKeyChanged    88      d8      �8          LOGICAL,    openDataQuery   |8      �8      �8    $      LOGICAL,INPUT pcPosition CHARACTER  openQuery   �8      �8      (9   	 2      LOGICAL,    prepareQuery    9      49      d9    <      LOGICAL,INPUT pcQuery CHARACTER rowAvailable    D9      �9      �9    I      LOGICAL,INPUT pcDirection CHARACTER rowValues   �9      �9      :   	 V      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcFormat CHARACTER,INPUT pcDelimiter CHARACTER    submitRow   �9      `:      �:   	 `      LOGICAL,INPUT pcRowIdent CHARACTER,INPUT pcValueList CHARACTER  updateRow   l:      �:      �:   	 j      LOGICAL,INPUT pcKeyValues CHARACTER,INPUT pcValueList CHARACTER getObjectType   �:      8;      h;    t      CHARACTER,  assignDBRow                             <  �;      ��                  �     <              8Ɣ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 4<  
         ��                            ����                            bufferCopyDBToRO                                4=  =      ��                      L=              �ʔ                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �=             d=  
             �� 
  �=             �=  
             ��   �=             �=               ��                  �=           ��                            ����                            compareDBRow                                �>  �>      ��                  	  
  �>              ՠ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmContinue                             �?  �?      ��                      �?              �ՠ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  @           ��                            ����                            dataAvailable                               A  �@      ��                       A               �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  8A           ��                            ����                            fetchDBRowForUpdate                             8B   B      ��                      PB              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchFirst                              8C   C      ��                      PC              �O�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchLast                               8D   D      ��                      PD              �P�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchNext                               8E   E      ��                      PE              �S�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchPrev                               8F   F      ��                     !  PF              �T�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            filterContainerHandler                              DG  ,G      ��                  #  %  \G              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 tG  
         ��                            ����                            initializeObject                                tH  \H      ��                  '  (  �H              $��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refetchDBRow                                xI  `I      ��                  *  ,  �I              \*�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �I  
         ��                            ����                            releaseDBRow                                �J  �J      ��                  .  /  �J              �.�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            startFilter                             �K  �K      ��                  1  2  �K              \/�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            transferDBRow                               �L  �L      ��                  4  7  �L              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   M             �L               ��                   M           ��                            ����                            addQueryWhere   H;      hM      �M    �      LOGICAL,INPUT pcWhere CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER    assignQuerySelection    xM      �M      $N    �      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER  bufferCompareDBToRO N      |N      �N    �      LOGICAL,INPUT phRowObjUpd HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER bufferWhereClause   �N      O      PO    �      CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcWhere CHARACTER  columnDataType  0O      �O      �O    �      CHARACTER,INPUT pcColumn CHARACTER  columnDbColumn  �O      �O      P    �      CHARACTER,INPUT pcColumn CHARACTER  columnQuerySelection    �O      4P      lP    �      CHARACTER,INPUT pcColumn CHARACTER  columnTable LP      �P      �P          CHARACTER,INPUT pcColumn CHARACTER  columnValMsg    �P      �P      Q           CHARACTER,INPUT pcColumn CHARACTER  dbColumnDataName    �P      4Q      hQ  !  *      CHARACTER,INPUT pcDbColumn CHARACTER    dbColumnHandle  HQ      �Q      �Q  "  ;      HANDLE,INPUT pcColumn CHARACTER excludeColumns  �Q      �Q      R  #  J      CHARACTER,INPUT iTable INTEGER  getDataColumns  �Q      0R      `R  $  Y      CHARACTER,  getForeignValues    @R      lR      �R  %  h      CHARACTER,  getQueryPosition    �R      �R      �R  &  y      CHARACTER,  getQuerySort    �R      �R      S  '  �      CHARACTER,  getQueryString  �R      (S      XS  (  �      CHARACTER,  getQueryWhere   8S      dS      �S  )  �      CHARACTER,  getTargetProcedure  tS      �S      �S  *  �      HANDLE, indexInformation    �S      �S      T  +  �      CHARACTER,INPUT pcQuery CHARACTER,INPUT plUseTableSep LOGICAL,INPUT pcIndexInfo CHARACTER   insertExpression    �S      lT      �T  ,  �      CHARACTER,INPUT pcWhere CHARACTER,INPUT pcExpression CHARACTER,INPUT pcAndOr CHARACTER  newQueryString  �T      �T      (U  -  �      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcAndOr CHARACTER  newQueryValidate    U      �U      �U  .  �      CHARACTER,INPUT pcQueryString CHARACTER,INPUT pcExpression CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcAndOr CHARACTER   newQueryWhere   �U      dV      �V  /  	      CHARACTER,INPUT pcWhere CHARACTER   newWhereClause  tV      �V      �V  0        CHARACTER,INPUT pcBuffer CHARACTER,INPUT pcExpression CHARACTER,INPUT pcWhere CHARACTER,INPUT pcAndOr CHARACTER refreshRowident �V      XW      �W  1  &      CHARACTER,INPUT pcRowident CHARACTER    removeForeignKey    hW      �W      �W  2  6      LOGICAL,    removeQuerySelection    �W      �W      (X  3  G      LOGICAL,INPUT pcColumns CHARACTER,INPUT pcOperators CHARACTER   resolveBuffer   X      hX      �X  4  \      CHARACTER,INPUT pcBuffer CHARACTER  rowidWhere  xX      �X      �X  5 
 j      CHARACTER,INPUT pcWhere CHARACTER   rowidWhereCols  �X      Y      <Y  6  u      CHARACTER,INPUT pcColumns CHARACTER,INPUT pcValues CHARACTER,INPUT pcOperators CHARACTER    setQueryPosition    Y      �Y      �Y  7  �      LOGICAL,INPUT pcPosition CHARACTER  setQuerySort    �Y      �Y       Z  8  �      LOGICAL,INPUT pcSort CHARACTER  setQueryString   Z      @Z      pZ  9  �      LOGICAL,INPUT pcQueryString CHARACTER   setQueryWhere   PZ      �Z      �Z  :  �      LOGICAL,INPUT pcWhere CHARACTER whereClauseBuffer   �Z      �Z      [  ;  �      CHARACTER,INPUT pcWhere CHARACTER   bindServer                              �[  �[      ��                  �  �  �[              �x�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �\  �\      ��                  �  �  �\              Dy�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �]  �]      ��                  �  �  �]              0V�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �^  �^      ��                  �  �  �^              (W�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �_  �_      ��                  �  �  `              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �`  �`      ��                  �  �  a              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �a  �a      ��                  �  �  b              ԣ�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 ,b  
         ��                            ����                            startServerObject                               ,c  c      ��                  �  �  Dc              T��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                0d  d      ��                  �  �  Hd              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  `d           ��                            ����                            getAppService   �Z      �d      �d  <  �      CHARACTER,  getASBound  �d      e      0e  = 
 �      LOGICAL,    getAsDivision   e      <e      le  >  �      CHARACTER,  getASHandle Le      xe      �e  ?  �      HANDLE, getASHasStarted �e      �e      �e  @        LOGICAL,    getASInfo   �e      �e      f  A 	       CHARACTER,  getASInitializeOnRun    �e       f      Xf  B        LOGICAL,    getASUsePrompt  8f      df      �f  C  3      LOGICAL,    getServerFileName   tf      �f      �f  D  B      CHARACTER,  getServerOperatingMode  �f      �f      g  E  T      CHARACTER,  runServerProcedure  �f      $g      Xg  F  k      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService   8g      �g      �g  G  ~      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �g      �g      $h  H  �      LOGICAL,INPUT pcDivision CHARACTER  setASHandle h      Hh      th  I  �      LOGICAL,INPUT phASHandle HANDLE setASInfo   Th      �h      �h  J 	 �      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �h      �h      i  K  �      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �h      <i      li  L  �      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   Li      �i      �i  M  �      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �i      �i      j  N  �      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �j  �j      ��                  �  �  �j              d�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  <k             k  
             ��   dk             0k               �� 
                 Xk  
         ��                            ����                            addMessage                              Pl  8l      ��                  �  �  hl              |N�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �l             �l               ��   �l             �l               ��                  �l           ��                            ����                            adjustTabOrder                              �m  �m      ��                  �  �  �m              4��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0n             �m  
             �� 
  Xn             $n  
             ��                  Ln           ��                            ����                            applyEntry                              Do  ,o      ��                  �  �  \o              �Ě                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  to           ��                            ����                            changeCursor                                pp  Xp      ��                  �  �  �p              `��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �p           ��                            ����                            createControls                              �q  �q      ��                  �  �  �q              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �r  �r      ��                  �  �  �r              X"�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �s  �s      ��                  �  �  �s              l#�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �t  �t      ��                  �  �  �t              X&�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �u  �u      ��                  �  �  �u               '�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �v  �v      ��                  �  �  �v              (*�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �w  �w      ��                  �  �  �w              �*�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �x  �x      ��                  �  �  �x              p0                     O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $y             �x  
             ��   Ly             y               ��   ty             @y               ��                  hy           ��                            ����                            modifyUserLinks                             dz  Lz      ��                  �  �  |z              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �z             �z               ��   �z             �z               �� 
                 �z  
         ��                            ����                            removeAllLinks                              �{  �{      ��                  �  �  �{              �q�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �|  �|      ��                  �  �  �|              t�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  D}             }  
             ��   l}             8}               �� 
                 `}  
         ��                            ����                            repositionObject                                `~  H~      ��                  �  �  x~              |{�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �~             �~               ��                  �~           ��                            ����                            returnFocus                             �  �      ��                  �  �  �              4��                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �  
         ��                            ����                            showMessageProcedure                                �  ̀      ��                  �     ��              H��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   H�             �               ��                  <�           ��                            ����                            toggleData                              4�  �      ��                      L�              (��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d�           ��                            ����                            viewObject                              \�  D�      ��                      t�              x��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �i      ̃      ��  O 
 K      LOGICAL,    assignLinkProperty  ؃      �      8�  P  V      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   �      ��      ��  Q  i      CHARACTER,  getChildDataKey ��      ̄      ��  R  w      CHARACTER,  getContainerHandle  ܄      �      <�  S  �      HANDLE, getContainerHidden  �      D�      x�  T  �      LOGICAL,    getContainerSource  X�      ��      ��  U  �      HANDLE, getContainerSourceEvents    ��      ��      ��  V  �      CHARACTER,  getContainerType    ܅      �      <�  W  �      CHARACTER,  getDataLinksEnabled �      H�      |�  X  �      LOGICAL,    getDataSource   \�      ��      ��  Y  �      HANDLE, getDataSourceEvents ��      ��      �  Z        CHARACTER,  getDataSourceNames  Ԇ       �      4�  [         CHARACTER,  getDataTarget   �      @�      p�  \  3      CHARACTER,  getDataTargetEvents P�      |�      ��  ]  A      CHARACTER,  getDBAware  ��      ��      �  ^ 
 U      LOGICAL,    getDesignDataObject ȇ      �      (�  _  `      CHARACTER,  getDynamicObject    �      4�      h�  `  t      LOGICAL,    getInstanceProperties   H�      t�      ��  a  �      CHARACTER,  getLogicalObjectName    ��      ��      ��  b  �      CHARACTER,  getLogicalVersion   Ј      ��      0�  c  �      CHARACTER,  getObjectHidden �      <�      l�  d  �      LOGICAL,    getObjectInitialized    L�      x�      ��  e  �      LOGICAL,    getObjectName   ��      ��      �  f  �      CHARACTER,  getObjectPage   ̉      ��      (�  g  �      INTEGER,    getObjectParent �      4�      d�  h        HANDLE, getObjectVersion    D�      l�      ��  i        CHARACTER,  getObjectVersionNumber  ��      ��      �  j  $      CHARACTER,  getParentDataKey    Ċ      ��      $�  k  ;      CHARACTER,  getPassThroughLinks �      0�      d�  l  L      CHARACTER,  getPhysicalObjectName   D�      p�      ��  m  `      CHARACTER,  getPhysicalVersion  ��      ��      �  n  v      CHARACTER,  getPropertyDialog   ȋ      �      (�  o  �      CHARACTER,  getQueryObject  �      4�      d�  p  �      LOGICAL,    getRunAttribute D�      p�      ��  q  �      CHARACTER,  getSupportedLinks   ��      ��      ��  r  �      CHARACTER,  getTranslatableProperties   ��      �      (�  s  �      CHARACTER,  getUIBMode  �      4�      `�  t 
 �      CHARACTER,  getUserProperty @�      l�      ��  u  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    |�      č      ��  v  	      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ܍      $�      P�  w  	      CHARACTER,INPUT pcLink CHARACTER    linkProperty    0�      t�      ��  x  "	      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      ��      �  y  /	      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �      x�      ��  z  ;	      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      ̏      ��  {  I	      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ܏      $�      T�  |  V	      CHARACTER,  setChildDataKey 4�      `�      ��  }  e	      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  p�      ��      �  ~  u	      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ̐      �      @�    �	      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents     �      `�      ��  �  �	      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled |�      ��      ��  �  �	      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ԑ      �      L�  �  �	      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents ,�      l�      ��  �  �	      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      Ȓ      ��  �  �	      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ܒ      $�      T�  �  �	      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 4�      x�      ��  �  
      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      Г      ��  � 
 
      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ܓ      �      P�  �  *
      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    0�      x�      ��  �  >
      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      Ȕ       �  �  O
      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    ��      $�      \�  �  e
      LOGICAL,INPUT c CHARACTER   setLogicalVersion   <�      x�      ��  �  z
      LOGICAL,INPUT cVersion CHARACTER    setObjectName   ��      Е       �  �  �
      LOGICAL,INPUT pcName CHARACTER  setObjectParent ��       �      P�  �  �
      LOGICAL,INPUT phParent HANDLE   setObjectVersion    0�      p�      ��  �  �
      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    ��      ̖       �  �  �
      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks ��      (�      \�  �  �
      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   <�      |�      ��  �  �
      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ��      ԗ      �  �  �
      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute �      ,�      \�  �  	      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   <�      ��      ��  �        LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ܘ      �  �  +      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      <�      h�  � 
 E      LOGICAL,INPUT pcMode CHARACTER  setUserProperty H�      ��      ��  �  P      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      $�  �  `      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      H�      t�  � 	 l      CHARACTER,INPUT pcName CHARACTER    l�      ��  0�      �       4   �����                 @�                      ��                    K                  ��q                         Ě          \�  ؛      �       4   �����                 �                      ��                     J                  H�q                          l�  �    7  �  ��      �       4   �����                 ��                      ��                  C  E                  ��q                       C  �         D                                  ,     
                    � ߱        �  $  G  ��  ���                           $  I  @�  ���                       x                         � ߱        x�    O  ��  �      �      4   �����                �                      ��                  P  	                  ��q                       P  ��  H�  o   S      ,                                 ��  $   T  t�  ���                       �  @         �              � ߱        ��  �   U        Ȟ  �   V  �      ܞ  �   X        �  �   Z  x      �  �   \  �      �  �   ^  `      ,�  �   _  �      @�  �   `        T�  �   c  �      h�  �   e         |�  �   f  |      ��  �   h  �      ��  �   i  t      ��  �   j  �      ̟  �   k  ,      ��  �   l  �      ��  �   r  �      �  �   t  P	      �  �   z  �	      0�  �   |   
      D�  �   ~  t
      X�  �     �
      l�  �   �  l      ��  �   �  �      ��  �   �  \      ��  �   �  �      ��  �   �  D      Р  �   �  �      �  �   �  �      ��  �   �  0      �  �   �  �       �  �   �  �      4�  �   �        H�  �   �  X      \�  �   �  �      p�  �   �        ��  �   �  L      ��  �   �  �      ��  �   �  �      ��  �   �         ԡ  �   �  <      �  �   �  x      ��  �   �  �      �  �   �  �          �   �  ,                      <�          ��  ��      ��                  ;	  i	  ��              8�q                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                                     (                         � ߱        h�  $ O	  آ  ���                           O   g	  ��  ��  h               ԣ          ģ  ̣    ��                                             ��                            ����                                8;      $�      ��     V     ܣ                       أ  t                     8�    �	  ��  �      t      4   ����t                 �                      ��                  �	  
                  DD�                       �	  ��  4�  �   �	  �      H�  �   �	  H      \�  �   �	  �      p�  �   �	  @      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ��  �   �	  (      ԥ  �   �	  �      �  �   �	         ��  �   �	  �      �  �   �	        $�  �   �	  �          �   �	        ��    @
  T�  Ц      x      4   ����x                �                      ��                  A
  �
                  Dl                       A
  d�  ��  �   C
  �      �  �   D
  T      �  �   E
  �      0�  �   F
  D      D�  �   G
  �      X�  �   H
  �      l�  �   J
  p      ��  �   K
  �      ��  �   L
  X      ��  �   M
  �      ��  �   N
  �      Ч  �   O
  D       �  �   P
  �       ��  �   Q
  �       �  �   R
  x!       �  �   S
  �!      4�  �   T
  h"      H�  �   U
  �"      \�  �   V
  `#      p�  �   W
  �#      ��  �   X
  X$      ��  �   Y
  �$      ��  �   Z
  �$      ��  �   [
  L%      Ԩ  �   \
  �%      �  �   ]
  <&      ��  �   ^
  �&      �  �   _
  4'      $�  �   `
  �'      8�  �   a
  ,(      L�  �   b
  h(      `�  �   d
  �(      t�  �   e
  X)      ��  �   f
  �)      ��  �   g
  *      ��  �   h
  �*      ĩ  �   i
  �*      ة  �   j
  l+      �  �   k
  �+       �  �   l
  \,      �  �   m
  �,      (�  �   n
  L-      <�  �   o
  �-      P�  �   p
  <.      d�  �   q
  �.      x�  �   r
  4/      ��  �   s
  �/          �   t
  $0      d�    �
  ��  8�      T0      4   ����T0                H�                      ��                  �
  }                  D��                       �
  ̪  \�  �   �
  �0      p�  �   �
  (1      ��  �   �
  �1      ��  �   �
  2      ��  �   �
  �2      ��  �   �
  3      ԫ  �   �
  |3      �  �   �
  �3      ��  �   �
  t4      �  �   �
  �4      $�  �   �
  l5      8�  �   �
  �5      L�  �   �
  d6      `�  �   �
  �6      t�  �      L7      ��  �     �7      ��  �     <8      ��  �     �8      Ĭ  �     ,9      ج  �     �9      �  �     :       �  �     X:      �  �     �:      (�  �   	  H;      <�  �   
  �;      P�  �     8<          �     �<      x�    �  ��  ��      =      4   ����=  	              �                      ��             	     �  ,                  ���                       �  ��   �  �   �  |=      4�  �   �  �=      H�  �   �  t>      \�  �   �  �>      p�  �   �  l?      ��  �   �  �?      ��  �   �  \@      ��  �   �  �@      ��  �   �  TA      Ԯ  �   �  �A      �  �   �  DB      ��  �   �  �B      �  �   �  <C      $�  �   �  �C      8�  �   �  ,D      L�  �   �  �D      `�  �   �  $E      t�  �   �  �E      ��  �   �  F      ��  �   �  �F      ��  �   �  G      į  �   �  �G      د  �   �  �G      �  �   �  8H       �  �   �  �H      �  �   �  0I      (�  �   �  �I      <�  �   �   J          �   �  �J      getRowObjUpdStatic  deleteRecordStatic  �    �  ��  ��      K      4   ����K      /   �  а     �                          3   ����K             �                      3   ����<K  Զ    �  ,�  ��  �  XK      4   ����XK  
              ��                      ��             
     �  N                  ȝ�                       �  <�  ̱  �   �  �K      $�  $  �  ��  ���                       �K     
                    � ߱        8�  �   �  L      ��  $   �  d�  ���                       ,L  @         L              � ߱        L�  $  �  ��  ���                       �L       	       	           � ߱        �M     
                N                     \O  @        
 O              � ߱        ܳ  V     �  ���                        hO       	       	       �O       
       
       �O       	       	           � ߱        l�  $    x�  ���                       �P     
                Q                     dR  @        
 $R              � ߱            V   /  �  ���                                      ̵                      ��                  P  �                  ��                       P  ��  pR     
                �R                     <T  @        
 �S          �T  @        
 dT          U  @        
 �T          dU  @        
 $U              � ߱            V   e  �  ���                        adm-clone-props �  ��              �     W     `                          \  p                     start-super-proc    �  d�  �           �     X                                  �                     l�      �   �      �X      4   �����X      /     ,�     <�                          3   ���� Y            \�                      3   ���� Y  ķ  $     ��  ���                       @Y                         � ߱        ��    0  �  \�  ��  \Y      4   ����\Y                и                      ��                  1  5                  �                       1  �  pY                     �Y                     �Y                         � ߱            $  2  l�  ���                             6  �  T�      �Y      4   �����Y  �Y                         � ߱            $  7  (�  ���                       |�    >  ��  ��  �  �Y      4   �����Y      $  ?  ع  ���                       Z                         � ߱            �   \  Z      XZ     
                �Z                     $\  @        
 �[              � ߱        ��  V   p  �  ���                        ��  �   �  0\      ��    "  غ  �      p\      4   ����p\      /   #  �     $�                          3   �����\            D�                      3   �����\  �\     
                <]                     �^  @        
 L^              � ߱        H�  V   /  T�  ���                        �^     
                T_                     �`  @        
 d`              � ߱        t�  V   S  �  ���                        ��    �  ��  �      �`      4   �����`                �                      ��                  �  �                  ,p                       �  ��  ��  /   �  H�     X�                          3   �����`            x�                      3   �����`      /   �  ��     Ľ                          3   ����a            �                      3   ����$a   �  /  @   �         Xa                      3   ����@a  initProps   x�  0�                   Y     �             �          �    	                                   t�          �  �      ��                �  �  4�              @�^                    O   ����    e�          O   ����    R�          O   ����    ��                            L�          ��  p   �  (|  ��      �  ��  �     4|                �                      ��                  �  �                  4��                       �  ��  4�  :  �                 $  �  `�  ���                       H|                         � ߱        �  �     `|                                        ��                  �  �                  P��                       �  ��  ��  ��     t|                                        ��                  �                     ��                       �  (�  0�   �     �|                                        ��                    !                  ,��                         ��  ��  ��     �|                                        ��                  "  >                  ���                       "  @�  H�  8�     �|                                        ��                  ?  [                  ĵ�                       ?  ��  ��  ��     �|                                        ��                  \  x                  ���                       \  X�  `�  P�     �|                                        ��                  y  �                  d��                       y  ��  ��  ��     �|  	                                      ��             	     �  �                  h �                       �  p�  x�  h�      }  
                                      ��             
     �  �                  8!�                       �  ��  �  ��     }                                        ��                  �  �                  "�                       �  ��  ��  ��     (}                                        ��                  �  	                  �"�                       �  �  �  �     <}                                        ��                  
  &                  ,@�                       
  ��  ��  ��     P}                                        ��                  '  C                  �@�                       '  ,�  4�  $�     d}                                        ��                  D  `                  xA�                       D  ��  ��  ��     x}                                        ��                  a  }                  HB�                       a  D�  L�  <�     �}                                        ��                  ~  �                  C�                       ~  ��      ��     �}                                        ��                  �  �                  ,D�                       �  \�      O   �  ��  ��  �}               \�          D�  P�   , $�                                                       �     ��                            ����                            <�  d�  X�  ��      ��     Z     d�                      � `�  !                     ��    �  �  ��      �}      4   �����}                ��                      ��                  �  �                  lE�                       �  ,�  �  /   �  ��     ��                          3   �����}            �                      3   �����}  ��  /   �  @�     P�                          3   ����~            p�                      3   ����(~  ��  /   �  ��     ��                          3   ����D~            ��                      3   ����d~      /   �  �     (�                          3   �����~            H�                      3   �����~  �~     
                @                     ��  @        
 P�              � ߱        ��  V   2  X�  ���                        ��  $  L  �  ���                       ��                         � ߱        Ȁ     
                D�                     ��  @        
 T�              � ߱        ��  V   V  @�  ���                        ��  $  p  ��  ���                       ��     
                    � ߱        ��     
                0�                     ��  @        
 @�              � ߱        ��  V   z  (�  ���                        t�  $  �  ��  ���                       ��     
                    � ߱        ��     
                �                     l�  @        
 ,�              � ߱        ��  V   �  �  ���                        \�  $  �  ��  ���                       ��                         � ߱        ��     
                (�                     x�  @        
 8�              � ߱        ��  V   �  ��  ���                        ��  �   �  ��      X�  $  �  ��  ���                       ��     
                    � ߱        Ĉ     
                @�                     ��  @        
 P�              � ߱        ��  V   �  ��  ���                        ��  $    ��  ���                       ��     
                    � ߱        ��  �     ��      H�  $  &  �  ���                       ��     
                    � ߱        \�  �   @  �      ��  $  b  ��  ���                       D�                         � ߱              m  ��  ��      `�      4   ����`�      /   n  �     �                          3   ������  L�     
   <�                      3   ������  |�        l�                      3   ������  ��        ��                      3   ������            ��                      3   ����؋  pushRowObjUpdTable  ��  ��  �                   [      �                               V                     pushTableAndValidate    ��  L�  �           |     \     �                          �  s                     remoteCommit    d�  ��  �           p     ]     �                          �  �                     serverCommit    ��  ,�  �           l     ^     �                          �  �                                     L�          �  �      ��                  �  �  4�              ̼�                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  �    ��                            ����                            <�  P�      ��              _      d�                      
�     �                     bufferCommit    ��   �  �           �      `                                                      disable_UI  �  l�                      a      �                                 
                    �  �    ����  �       ��          �  8   ����   ,�  8   ����   <�  8   ����   L�  8   ����       8   ����       8   ����       l�  x�      viewObject  ,   \�  ��  ��      toggleData  ,INPUT plEnabled LOGICAL    |�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��   �  ,�      returnFocus ,INPUT hTarget HANDLE   �  T�  h�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    D�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  �      removeAllLinks  ,   ��  (�  8�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  �  (�      hideObject  ,   �  <�  H�      exitObject  ,   ,�  \�  t�      editInstanceProperties  ,   L�  ��  ��      displayLinks    ,   x�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  �  �      applyEntry  ,INPUT pcField CHARACTER    ��  D�  T�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER 4�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �  �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE  �  l�  |�      unbindServer    ,INPUT pcMode CHARACTER \�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  ��      disconnectObject    ,   ��  �  �      destroyObject   ,   ��  ,�  8�      bindServer  ,   �  L�  \�      transferDBRow   ,INPUT pcRowIdent CHARACTER,INPUT piRowNum INTEGER  <�  ��  ��      startFilter ,   ��  ��  ��      releaseDBRow    ,   ��  ��  ��      refetchDBRow    ,INPUT phRowObjUpd HANDLE   ��   �  8�      filterContainerHandler  ,INPUT phFilterContainer HANDLE �  h�  |�      fetchDBRowForUpdate ,   X�  ��  ��      confirmContinue ,INPUT-OUTPUT pioCancel LOGICAL ��  ��  ��      compareDBRow    ,   ��  ��  �      bufferCopyDBToRO    ,INPUT phRowObj HANDLE,INPUT phBuffer HANDLE,INPUT pcExcludes CHARACTER,INPUT pcAssigns CHARACTER   ��  |�  ��      assignDBRow ,INPUT phRowObjUpd HANDLE   l�  ��  ��      updateState ,INPUT pcState CHARACTER    ��  ��   �      updateQueryPosition ,   ��  �  (�      updateAddQueryWhere ,INPUT pcWhere CHARACTER,INPUT pcField CHARACTER    �  l�  |�      undoTransaction ,   \�  ��  ��      transferToExcel ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT plUseExisting LOGICAL,INPUT piMaxRecords INTEGER  ��   �  8�      synchronizeProperties   ,INPUT pcPropertiesForServer CHARACTER,OUTPUT pcPropertiesForClient CHARACTER   �  ��  ��      submitValidation    ,INPUT pcValueList CHARACTER,INPUT pcUpdColumns CHARACTER   ��  ��  �      submitForeignKey    ,INPUT pcRowIdent CHARACTER,INPUT-OUTPUT pcValueList CHARACTER,INPUT-OUTPUT pcUpdColumns CHARACTER  ��  ��  ��      submitCommit    ,INPUT pcRowIdent CHARACTER,INPUT plReopen LOGICAL  p�  ��  ��      startServerObject   ,   ��  ��  �      setPropertyList ,INPUT pcProperties CHARACTER   ��  <�  X�      serverFetchRowObjUpdTable   ,OUTPUT TABLE-HANDLE phRowObjUpd    ,�  ��  ��      serverSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject    |�  T�  l�      saveContextAndDestroy   ,OUTPUT pcContext CHARACTER D�  ��  ��      rowObjectState  ,INPUT pcState CHARACTER    ��  ��  ��      retrieveFilter  ,   ��  ��  �      restartServerObject ,   ��   �  0�      remoteSendRows  ,INPUT-OUTPUT piocContext CHARACTER,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT pioRowsReturned INTEGER,OUTPUT TABLE-HANDLE phRowObject,OUTPUT pocMessages CHARACTER   �  (�  4�      refreshRow  ,   �  H�  X�      printToCrystal  ,INPUT pcFieldList CHARACTER,INPUT plIncludeObj LOGICAL,INPUT piMaxRecords INTEGER  8�  ��  ��      isUpdatePending ,INPUT-OUTPUT plUpdate LOGICAL  ��  ��  �      initializeServerObject  ,   ��  (�  <�      initializeObject    ,   �  P�  X�      home    ,   @�  l�  |�      genContextList  ,OUTPUT pcContext CHARACTER \�  ��  ��      fetchPrev   ,   ��  ��  ��      fetchNext   ,   ��  ��  ��      fetchLast   ,   ��  �  �      fetchFirst  ,   ��  (�  4�      fetchBatch  ,INPUT plForwards LOGICAL   �  `�  x�      endClientDataRequest    ,   P�  ��  ��      destroyServerObject ,   |�  ��  ��      describeSchema  ,INPUT pcSdoName CHARACTER,OUTPUT TABLE-HANDLE hTtSchema    ��  �   �      dataAvailable   ,INPUT pcRelative CHARACTER  �  L�  X�      copyColumns ,INPUT pcViewColList CHARACTER,INPUT phDataQuery HANDLE <�  ��  ��      commitTransaction   ,   ��  ��  ��      clientSendRows  ,INPUT piStartRow INTEGER,INPUT pcRowIdent CHARACTER,INPUT plNext LOGICAL,INPUT piRowsToReturn INTEGER,OUTPUT piRowsReturned INTEGER    ��  p�  ��      batchServices   ,INPUT pcServices CHARACTER,OUTPUT pcValues CHARACTER        � 
"     
 �%     adecomm/as-utils.w  
"   
   �    }        �
"     
    �     }        �� !   D   %               � 
" 
   
 � %              h �P  \         (          
�                          
�            � v   �
" 
   
 q
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        �    7%               
"   
 ��               1� �  
 �� �   � %               o%   o           � �    �
"   
 ��           �    1� �   �� �   � %               o%   o           � �   �
"   
 ��           �    1� �  
 �� �   � %               o%   o           � �   �
"   
 ��           l    1� �   �� �   � %               o%   o           � �    �
"   
 ��           �    1� �   �� �   � %               o%   o           � �   �
"   
 ��           T    1� �   ��    � %               o%   o           %               
"   
 � �          �    1�    � �      
"   
 ��               1� $   �� �   � %               o%   o           � 7  �
"   
 ��           �    1� 9   �� �   � %               o%   o           � H  S �
"   
 ��           �    1� �   ��    � %               o%   o           %               
"   
 ��           p    1� �   ��    � %               o%   o           %               
"   
 ��           �    1� �   ��    � %               o%   o           %              
"   
 � �          h    1� �   � �      
"   
 ��           �    1� �  
 ��    � %               o%   o           %               
"   
 ��                1� �   �� �   � %               o%   o           � �    �
"   
 � �          �    1� �   � �      
"   
 ��           �    1� �   �� �   � %               o%   o           �   t �
"   
 � �          D	    1� �  
 � �      
"   
 ��           �	    1� �   �� �   � %               o%   o           � �  � �
"   
 ��           �	    1� 1   �� �   � %               o%   o           � �    �
"   
 ��           h
    1� H  
 �� S   � %               o%   o           %               
"   
 ��           �
    1� W   ��    � %               o%   o           %              
"   
 q�           `    1� _   q� �   � %               o%   o           � �    �
"   
 q�           �    1� p   q� �   � %               o%   o           o%   o           
"   
 q�           P    1� �  
 q� �   � %               o%   o           � �    �
"   
 q�           �    1� �   q� �  	 � %               o%   o           � �  / q
"   
 � �          8    1� �   � � �  	   
"   
 ��           t    1� �   �� �  	 � o%   o           o%   o           � �    �
"   
 � �          �    1� �   � � �  	   
"   
 ��           $    1� 
   �� �  	 � o%   o           o%   o           � �    �
"   
 � �          �    1�    � �      
"   
 � �          �    1� (   � � �  	   
"   
 � �              1� 5   � � �  	   
"   
 � �          L    1� B   � � �  	   
"   
 q�           �    1� P   q�    � o%   o           o%   o           %              
"   
 � �              1� a   � � �  	   
"   
 � �          @    1� o  
 � � z     
"   
 � �          |    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          0    1� �   � � �  	   
"   
 � �          l    1� �  	 � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 � �          �    1� �   � � �  	   
"   
 q�                1� �   q� �   � %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 
"   
   
"   
 �(�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�            ��      p�               �L
�    %              � 8          � $         �           
�    � 2     
"   
 �� @  , 
�           �� �  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� 5  
 � �   � %               o%   o           � �    
"   
 �           <    1� @  
 � �   � %               o%   o           o%   o           
"   
 ��           �    1� K   ��    � %               o%   o           o%   o           
"   
 q�           4    1� T   q�    � %               o%   o           %               
"   
 ��           �    1� c   ��    � %               o%   o           %               
"   
 ��           ,    1� p   �� �   � %               o%   o           � �    �
"   
 q�           �    1� w   q�    � %               o%   o           %              
"   
 q�               1� �   q�    � %               o%   o           o%   o           
"   
 q�           �    1� �   q� �   � %               o%   o           o%   o           
"   
 ��               1� �  	 �� �   � %               o%   o           � �    �
"   
 ��           �    1� �   �� �   � %               o%   o           o%   o           
"   
 ��               1� �   �� �   � %               o%   o           o%   o           
"   
 ��           �    1� �   ��    � %               o%   o           %               
"   
 ��           �    1� �   ��    � %               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 �           �    1� �  
 �    � %               o%   o           %              
"   
 �           H    1� �   � �   � %               o%   o           o%   o           
"   
 �           �    1�    � �   � %               o%   o           � �    q
"   
 �           8    1�    � �   � %               o%   o           o%   o           
"   
 � �          �    1�    � �      
"   
 q�           �    1� *   q� �   � %               o%   o           � =  ! �
"   
 q�           d    1� _   q� �   � %               o%   o           � �    q
"   
 ��           �    1� l   �� �   � %               o%   o           �    q
"   
 � �          L    1� �   � � �     
"   
 � �          �    1� �   � �      
"   
 ��           �    1� �   �� �   � %               o%   o           � �    �
"   
 � �          8     1� �  
 � �      
"   
 ��           t     1� �   ��    � %               o%   o           o%   o           
"   
 q�           �     1� �   q�    � %               o%   o           %               
"   
 �           l!    1� �   �    � %               o%   o           %               
"   
 q�           �!    1� �   q� �   � %               o%   o           � �    
"   
 q�           \"    1�    q� �   � %               o%   o           o%   o           
"   
 �           �"    1�    �    � %               o%   o           %              
"   
 q�           T#    1� %   q�    � %               o%   o           %               
"   
 ��           �#    1� 2   ��    � %               o%   o           %               
"   
 � �          L$    1� B   � �      
"   
 � �          �$    1� O   � � �     
"   
 q�           �$    1� \   q� S   � %               o%   o           o%   o           
"   
 q�           @%    1� h   q� �   � %               o%   o           � �    �
"   
 q�           �%    1� v   q� �   � %               o%   o           o%   o           
"   
 �           0&    1� �   �    � o%   o           o%   o           o%   o           
"   
 �           �&    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           ('    1� �   � �   � %               o%   o           o%   o           
"   
 ��           �'    1� �  
 �� S   � %               o%   o           o%   o           
"   
 � �           (    1� �   � � �     
"   
 q�           \(    1� �   q� �   � %               o%   o           � �  4 
"   
 q�           �(    1�   
 q�    � %               o%   o           %              
"   
 � �          L)    1� *   � �      
"   
 ��           �)    1� ;   �� �   � %               o%   o           � �    �
"   
 ��           �)    1� I   ��    � %               o%   o           %              
"   
 q�           x*    1� X   q� �   � %               o%   o           � �    �
"   
 ��           �*    1� e   �� �   � %               o%   o           � �    q
"   
 �           `+    1� s   � �   � %               o%   o           � �    �
"   
 �           �+    1�    �    � %               o%   o           %               
"   
 �           P,    1� �  	 �    � %               o%   o           o%   o           
"   
 ��           �,    1� �   �� �   � %               o%   o           � �  	 q
"   
 ��           @-    1� �   �� S   � %               o%   o           %       �       
"   
 ��           �-    1� �   �� �   � %               o%   o           � �    �
"   
 ��           0.    1� �   ��    � o%   o           o%   o           %              
"   
 ��           �.    1� �   ��    � %               o%   o           %               
"   
 ��           (/    1� �   �� �   � %               o%   o           o%   o           
"   
 �           �/    1� �   � �  	 � %               o%   o           � �    
"   
 � �          0    1�    � � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           �0    1�   
 �� �   � %               o%   o           � �    �
"   
 ��           1    1� '   ��    � %               o%   o           %               
"   
 ��           �1    1� 4  	 �� �   � %               o%   o           � �    �
"   
 q�           2    1� >   q� �   � %               o%   o           � �    �
"   
 ��           �2    1� L   ��    � %               o%   o           %               
"   
 �           �2    1� \   � �   � %               o%   o           � �    �
"   
 �           p3    1� o   � �   � %               o%   o           o%   o           
"   
 �           �3    1� w   � �   � %               o%   o           o%   o           
"   
 q�           h4    1� �   q�    � %               o%   o           o%   o           
"   
 ��           �4    1� �   ��    � %               o%   o           o%   o           
"   
 ��           `5    1� �   ��    � %               o%   o           o%   o           
"   
 ��           �5    1� �   �� �   � %               o%   o           o%   o           
"   
 ��           X6    1� �  	 �� �  	 � %               o%   o           � �    q
"   
 q�           �6    1� �  
 q� �  	 � %               o%   o           � �    �
"   
 �           @7    1� �   � �   � %               o%   o           � �    q
"   
 �           �7    1� �   � �   � %               o%   o           o%   o           
"   
 ��           08    1� �   �� �   � %               o%   o           o%   o           
"   
 �           �8    1�    � �   � %               o%   o           � �    
"   
 ��            9    1�    �� �   � %               o%   o           � �    
"   
 ��           �9    1� %   �� �  	 � %               o%   o           o%   o           
"   
 � �          :    1� 7   � �      
"   
 q�           L:    1� C   q� �   � %               o%   o           � �    �
"   
 q�           �:    1� Q   q� �   � %               o%   o           o%   o           
"   
 ��           <;    1� d   ��    � %               o%   o           o%   o           
"   
 �           �;    1� v  
 � �   � %               o%   o           � �    �
"   
 ��           ,<    1� �   �� �   � %               o%   o           � �    
"   
 q�           �<    1� �   q�    � %               o%   o           %               P �L 
�H T   %              �     }        �GG %              
"   
 q�           p=    1� �  	 q�    � %               o%   o           o%   o           
"   
 q�           �=    1� �   q�    � %               o%   o           o%   o           
"   
 �           h>    1� �   �    � %               o%   o           o%   o           
"   
 ��           �>    1� �   ��    � %               o%   o           %              
"   
 ��           `?    1� �   �� �   � %               o%   o           � �  M �
"   
 �           �?    1� M   �    � %               o%   o           %              
"   
 ��           P@    1� ^   ��    � %               o%   o           %               
"   
 ��           �@    1� r   ��    � %               o%   o           %               
"   
 ��           HA    1� �   �� �  	 � %               o%   o           � �   �
"   
 �           �A    1� �   �    � %               o%   o           %               
"   
 �           8B    1� �   � �  	 � %               o%   o           o%   o           
"   
 �           �B    1� �   �    � o%   o           o%   o           %              
"   
 ��           0C    1� �   �� �  	 � o%   o           o%   o           � �    �
"   
 ��           �C    1� �   ��    � o%   o           o%   o           o%   o           
"   
 ��            D    1� �   ��    � o%   o           o%   o           o%   o           
"   
 ��           �D    1�    �� �  	 � o%   o           o%   o           o%   o           
"   
 ��           E    1�    ��    � o%   o           o%   o           o%   o           
"   
 ��           �E    1� *   �� �  	 � o%   o           o%   o           � 8   �
"   
 �           F    1� :   � �  	 � o%   o           o%   o           � I   
"   
 ��           |F    1� U   ��    � %               o%   o           %               
"   
 q�           �F    1� i   q�    � %               o%   o           %               
"   
 � �          tG    1� }   � � �  	   
"   
 q�           �G    1� �   q�    � %               o%   o           %               
"   
 q�           ,H    1� �   q� �   � %               o%   o           o%   o           
"   
 ��           �H    1� �   �� �   � %               o%   o           o%   o           
"   
 �           $I    1� �   �    � %               o%   o           o%   o           
"   
 ��           �I    1� �   �� �   � %               o%   o           � �    
"   
 �           J    1� �   � �   � %               o%   o           %               
"   
 q�           �J    1� �  	 q�    � %               o%   o           %                "    � %     start-super-proc v� %     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       �K    6�      
"   
   
�        �K    8
"   
   �        �K    ��     }        �G 4              
"   
 ߱G %              G %              %�   AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �M    ��    � P   �        �M    �@    
� @  , 
�       �M    ��    �p�               �L
�    %              � 8       N    � $         �           
�    � 2   �
"   
 �p� @  , 
�       O    �� $   �p�               �L"  	  , �   � 2   � 4   � �     }        �A      |    "  	    � 2   %              (<   \ (    |    �     }        �A� 6   �A"  
      "  	  �"  
    < "  	  �"  
  (    |    �     }        �A� 6   �A"  
  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �P    ��    � P   �        �P    �@    
� @  , 
�       �P    ��    �p�               �L
�    %              � 8      Q    � $         �           
�    � 2   �
"   
 �p� @  , 
�       R    �� �  
 �p�               �L"  	  , 
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        �R    ��    � P   �        �R    �@    
� @  , 
�       �R    ��      p�               �L
�    %              � 8      �R    � $         �           
�    � 2     
"   
 �p� @  , 
�       �S    �� �  
 �p�               �L%     SmartDataObject 
"   
   p� @  , 
�       XT    �� �     p�               �L%               
"   
  p� @  , 
�       �T    �� 
    p�               �L%               
"   
  p� @  , 
�       U    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        �U    ��    �
"   
   � 8      DV    � $         �           
�    � 2   �
"   
   �        �V    �
"   
   �       �V    /
"   
   
"   
   �       �V    6�      
"   
   
�        W    8
"   
   �        4W    �
"   
   �       TW    �
"   
   p�    � _   
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        X    �A"    �A
"   
   
�        dX    �@ � 
"   
 �"      �       }        �
"   
 � %              %                "    � %     start-super-proc v� %     adm2/appserver.p i�    � �     
�    �     }        �%               %      Server  - �     }        �    "    q� �    � %                   "    q� �    � %      NONE    p�,  8         $     "    �        � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �Z    ��    � P   �        �Z    �@    
� @  , 
�       �Z    ��    �p�               �L
�    %              � 8      �Z    � $         �           
�    � 2   �
"   
 �p� @  , 
�       �[    �� �   �p�               �L"    , p�,  8         $     "    �        �    �
�     "    � %     start-super-proc u� %     adm2/dataquery.p ��
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        ]    ��    � P   �        ]    �@    
� @  , 
�       $]    ��    �p�               �L
�    %              � 8      0]    � $         �    �     
�    � 2   �
"   
 �p� @  , 
�       @^    �� �   �p�               �L%H > 8   dataAvailable,confirmContinue,isUpdatePending,buildDataRequest  
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �        $_    ��    � P   �        0_    �@    
� @  , 
�       <_    ��    �p�               �L
�    %              � 8      H_    � $         �    �     
�    � 2   �
"   
 �p� @  , 
�       X`    �� �   �p�               �L%               "    � %     start-super-proc t� %     adm2/query.p ��%     start-super-proc t� %     adm2/queryext.p % 	    initProps �
�    %4 + $   FOR EACH Almacen NO-LOCK INDEXED-REPOSITION �   � u     � w     �       
�     	         �G
"   
 �        �a    �G
"   
   
"   
    x    (0 4      �        �a    �G%                   �        b    �GG %              � [    �� \         %              %                   "      %              
"   
       "      �        �b    �
"   
   �        (c    �
"   
   
�       Hc    �"       \      H   "    �((       "      %              � �      � u   �     
"   
   
"   
 �  \      H   "      ((       "      %              � �     � u   ��        �c    �%                   %              %                   "  (    %                  "  (        
"   
 �
"   
 �0 T       m � "  (  ��        �d    �A @   "      $         � "  (  �� 6   � �        e    �� "  (    
"   
 �  \ H     H   "      ((       "    �%              � �    � � u     (        "  !  �� �    ��        �e    �"  !  �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
   
"   
   (�  L ( l       �        �f    ��    � P   �        �f    �@    
� @  , 
�       �f    ��      p�               �L
�    %              � 8      �f    � $         �           
�    � 2     
"   
 �p� @  , 
�       �g    �� �   �p�               �L%               
"   
   p� @  , 
�       Dh    ��      p�               �L"    , �,  8         $     "    �L        � c  
  
�    
�H T   %              �     }        �GG %              
"   
 $ 
"   
   
"   
 �
"   
 �(�  L ( l       �        (i    ��    � P   �        4i    �@    
� @  , 
�       @i    ��    �p�               �L
�    %              � 8      Li    � $         �    �     
�    � 2     
"   
 �p� @  , 
�       \j    �� 7   �p�               �L
"   
 , 
"   
   p� @  , 
�       �j    ��      p�               �L"    , 
"   
  p� @  , 
�       k    �� �    p�               �L"    ,     "    � �    � %L B <   OPEN QUERY Query-Main FOR EACH Almacen NO-LOCK INDEXED-REPOSITION.      "    ac� �    I((        "    OS%                   "    � � �     "    � (   "           "    � %              @ �,  8         $     "    �        � �    
�    p�,  8         $     � �   �        � �   �
�    %               �    "      � w         %              %                   "      %                  "      "      "     T(        "    �%              "    �� w   � "      �       "    ��    "    �� 6   � � �      � 6   ��    "     � 6    S    "      "    �     "    %                � @    �     t T     P   4       � "      (0       4       "      � �      � �    �� u   T ,  %              T   "    "    � � w     � 6   �� u   T    �    "    � 6   � "      � 6   �"      %                   %              %                   "      %                  "      �     "      �     "       \      H   "      ((       "    �%              � �    � � �     4  q     "      
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �q    ��    � P   �        �q    �@    
� @  , 
�       �q    ��    �p�               �L
�    %              � 8      �q    � $         �           
�    � 2   �
"   
 �p� @  , 
�       �r    ��   
 �p�               �L"    ,       "  
  ��    � �   �� w   �       "  	    �    � �   � � w   ��   � u     � w     � �   ��   � u     � w   �� �   ��   � u     � w     � �     
�H T   %              �     }        �GG %              
"   
 � 
"   
 �
"   
 � 
"   
 � (�  L ( l       �        Dt    ��    � P   �        Pt    �@    
� @  , 
�       \t    ��    � p�               �L
�    %              � 8      ht    � $         �           
�    � 2     
"   
 �p� @  , 
�       xu    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �u    �� �     p�               �L"    , 
"   
  p� @  , 
�       (v    �� \    p�               �L"    ,     %              %                   "      %                  "      �     "      �     "      4 (        "  
    �    � �     � w         "  	  q�     "    �T    "      "      @ A,    �   � u   � � �     "    �"       T      @   "    � (        "      � �    �� �      � u   �"    �     "  	   %              D H   @ A,    �   � u   �� �     "    �"    u,    S   "    �� �   u� w   � %                T      @   "    � (        "      � �    �� �      � u   �"    �     "  
   %                         "    � � �     "    �           "      � �   �"      
�H T   %              �     }        �GG %              
"   
 ^
"   
   
"   
 ^
"   
 �(�  L ( l       �        Dz    ��    � P   �        Pz    �@    
� @  , 
�       \z    ��    ^p�               �L
�    %              � 8      hz    � $         �    �     
�    � 2   � 
"   
 �p� @  , 
�       x{    �� �   �p�               �L"    , 
"   
   p� @  , 
�       �{    �� \     p�               �L"    , "      %               �     }        �%              %              %              %              %              %              %              %       	       %       
       %              %              %              %              %              %              %              %              "       "    � %     start-super-proc s� %     adm2/data.p %     start-super-proc s� %     adm2/dataext.p %     start-super-proc s� %     adm2/dataextcols.p %     start-super-proc s� %     adm2/dataextapi.p �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
 �(�  L ( l       �            ��    � P   �            �@    
� @  , 
�       (    ��    �p�               �L
�    %              � 8      4    � $         �    �     
�    � 2   �
"   
 �p� @  , 
�       D�    �� �   �p�               �L%               %      "Util/xxx/dtables.i" t� 
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �    ��    � P   �         �    �@    
� @  , 
�       ,�    ��    �p�               �L
�    %              � 8      8�    � $         �           
�    � 2   �
"   
 �p� @  , 
�       H�    �� �   �p�               �L"    , 
�     	        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �         �    ��    � P   �        �    �@    
� @  , 
�       �    ��    �p�               �L
�    %              � 8      $�    � $         �           
�    � 2   �
"   
 �p� @  , 
�       4�    �� �  
 �p�               �L
"   
 , 
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �    ��    � P   �        ��    �@    
� @  , 
�       �    ��    �p�               �L
�    %              � 8      �    � $         �           
�    � 2   �
"   
 �p� @  , 
�        �    �� �  	 �p�               �L
"   
 , 
"   
 �      � �  	   �        x�    �
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        ��    ��    � P   �        �    �@    
� @  , 
�       �    ��    �p�               �L
�    %              � 8      �    � $         �           
�    � 2   �
"   
 �p� @  , 
�       ,�    �� �   �p�               �L"    , 
"   
   �       ��    �"      
�     
        �G
�H T   %              �     }        �GG %              
"   
 �
"   
 � 
"   
 �
"   
   (�  L ( l       �        �    ��    � P   �        �    �@    
� @  , 
�       (�    ��    �p�               �L
�    %              � 8      4�    � $         �           
�    � 2   �
"   
 �p� @  , 
�       D�    �� �  	 �p�               �L
"   
 , 
�             �Gp�,  8         $     
"   
 �        � 	   �
�    
�             �Gp�,  8         $     
"   
 �        �    �
�    �    � -     
�        "    �� �    � %     modifyListProperty  
�    %      REMOVE  %     SupportedLinks %     Update-Target  %     bufferValidate  
�    "      �  %      setContextAndInitialize 
�    "      %     bufferCommit    
�    "      "      �    � �     
�    %               %     bufferCommit    
�    "      "      
�     
        �G%      SUPER   "       "       "       �     }        �
�                    �           �   l       ��                 ]  �  �               X��                    O   ����    e�          O   ����    R�          O   ����    ��        $  l  �   ���                       �U     
                    � ߱              m  (  �      V      4   ����V                �                      ��                  n  �                  �.                       n  8  �  �  o  PV            q  �  `      �V      4   �����V                p                      ��                  r                    �/                       r  �  �  o   s      ,                                 �  �   t  �V      �  �   u  �V      $  $  v  �  ���                        W     
                    � ߱        8  �   w  @W      L  �   x  `W      `  �   {  �W          $   ~  �  ���                       �W  @         �W              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               p0                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       X     
                    � ߱                  �  �                      ��                   �  �                  t�                     �  4      4   ����$X      $  �  �  ���                       pX     
                    � ߱        �    �  4  D      �X      4   �����X      /  �  p                               3   �����X  �  �   �  �X          O   �  ��  ��  �X                               , �                          
                               �      ��                            ����                                            �           �   l       ��            	     L  �  �               �p                    O   ����    e�          O   ����    R�          O   ����    ��        $  l  �   ���                       `a                         � ߱        �  $  m  8  ���                       �a                         � ߱        �a     
                b  @         �a              � ߱        D  $   �  d  ���                         T      �  x                      ��        0         �  �                  \:u      4c     L     �  �      $  �  �  ���                       �b                         � ߱          $  �  �  ���                       �b                         � ߱            4   ���� c  Tc     
                tc                     �c                         � ߱          $  �    ���                                                            ��                  �  �                  \>u                �     �  �  �  $  �  L  ���                       pd       !       !           � ߱          �      L  �                      ��        0         �  �                  \Bu     ( �d            �  x      $  �     ���                       �d       (       (           � ߱        �  $  �  x  ���                       �d       (       (           � ߱            4   �����d        �  �  L      e      4   ����e                \                      ��                  �  �                  \Fu                       �  �  �  $  �  �  ���                       �e       !       !           � ߱            O   �  �� ��          $  �  �  ���                       �e                         � ߱        df     
                �f                     0h  @        
 �g          �h  @        
 Ph          �h                     �h     
                Xi                     �j  @        
 hj           k  @        
 �j          Xk  @        
 k              � ߱        x  V   �  $  ���                        P	    �  �  $	      dk      4   ����dk  �k                     �k                     �k                     Pl                         � ߱            $  �  �  ���                       �	    �  l	  |	      �l      4   �����l      �   �  �l      �	  $  �  �	  ���                       m                         � ߱        �
  $  �  
  ���                       $m                         � ߱          �
                              ��        0         �  �                  �Lu      �m     �     �  @
      $  �  �
  ���                       Dm                         � ߱        l  $  �  @  ���                       tm                         � ߱            4   �����m  �m                     n                     n                     ln                     �n                         � ߱        D  $  �  |  ���                             �  `  p      �n      4   �����n      $  �  �  ���                       �n           p             � ߱        �  $  �  �  ���                       p                         � ߱          �      �  \                      ��        0         �  �                  �Mu      �p          �         $  �  �  ���                        p                         � ߱        L  $  �     ���                       Pp                         � ߱            4   ����xp      $  �  �  ���                       �p                         � ߱        4q     
                �q                      s  @        
 �r              � ߱        �  V   �  �  ���                        s       
       
       @s       	       	       ts                     �s                         � ߱        �  $    D  ���                       �  $  �    ���                       �s                         � ߱        �s     
                tt                     �u  @        
 �u          v  @        
 �u          tv  @        
 4v              � ߱        |  V   �  H  ���                          �      �  \                      ��        0    	     *  ?                  ď^       w     4     *        $  *  �  ���                       �v                         � ߱        <  $  *    ���                       �v                         � ߱        L  4   �����v      4   ����w  �  $  /  �  ���                       xw                         � ߱        �    1  �  L      �w      4   �����w                �                      ��                  2  6                  P�^                       2  �  �w                     Dx       	       	           � ߱            $  3  \  ���                             8  �  h      lx      4   ����lx  	              �                      ��             	     :  >                  �^                       :  �   y                     hy       
       
           � ߱            $  ;  x  ���                       �y                     �y                         � ߱        �  $  E  �  ���                       �y     
                tz                     �{  @        
 �{          |  @        
 �{              � ߱            V   S  `  ���                                    J `          �  �  � X@                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
             
                                                                                                                                                                                                                               ) �   �   �   �       (  8  H  X  h  x  �  �  �  �  �  �      (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX     ) �   �   �   �      (  8  H  X  h  x  �  �  �  �  �   �     (  8  H  X  h  x  �  �  �  �  �  �  �  �      (  8  H  8HXhx��������(8HX  �   :                  � �                     �    ��                      ��                            ����                            �                          �l                                �   l       ��                       �               ��                    O   ����    e�          O   ����    R�          O   ����    ��              !                    ��                            ����                                            �           �   l       ��                    $  �               �	�                    O   ����    e�          O   ����    R�          O   ����    ��      i       �              �                  $                  d  /  !  $     4  �                      3   �����            T                      3   �����      O   "  ��  ��  $�               �          �  �    �                                             ��                            ����                                            H          �   l       ��                  .  Y  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �                $                  �       ,             �          �                                �  /  M  t     �  L�                      3   ����(�            �                      3   ����T�     /  O  �     �  |�                      3   ����`�  x                             3   ������      $   O  L  ���                                                   � ߱                  �  �                  3   ������      $   O  �  ���                                                   � ߱        X  $  S  ,  ���                       ��                         � ߱            O   W  ��  ��  ��               �          �  �   @ �                                                              0              0           ��                            ����                                                       �   l       ��                  c  �  �               ,��                    O   ����    e�          O   ����    R�          O   ����    ��      �       $                  �                    �          �                      �              /  �  L     \  �                      3   ����̌  �        |  �                  3   ������      $   �  �  ���                                                   � ߱                                      3   ������      $   �  @  ���                                                   � ߱                     �          �  �   , �                                                                 ��                            ����                                                      �   l       ��                 �  �  �               ,��                    O   ����    e�          O   ����    R�          O   ����    ��      �        �              �          �                       �          |  /   �  0     @                         3   �����  �        `  p                  3   ����0�      $   �  �  ���                                                    � ߱                  �  �                  3   ����<�      $   �  $  ���                                                    � ߱        H�                         � ߱            V   �  P  ���                                                �     , �                                                                  ��                            ����                                            �           �   l       ��                  �     �               �                    O   ����    e�          O   ����    R�          O   ����    ��            �  �   �       T�      4   ����T�      �   �  h�    ��                            ����                            TXS appSrvUtils s-codcia Almacen C:\newsie\on_in_co\Util\xxx\dtables.w should only be RUN PERSISTENT. ADDROW CANCELROW CANNAVIGATE CLOSEQUERY COLUMNPROPS COLVALUES COPYROW CREATEROW DELETEROW FETCHROW FETCHROWIDENT FINDROW FINDROWWHERE FIRSTROWIDS GETLASTCOMMITERRORTYPE HASFOREIGNKEYCHANGED OPENDATAQUERY OPENQUERY PREPAREQUERY ROWAVAILABLE ROWVALUES SUBMITROW UPDATEROW GETOBJECTTYPE xiRocketIndexLimit ADDQUERYWHERE ASSIGNQUERYSELECTION BUFFERCOMPAREDBTORO BUFFERWHERECLAUSE COLUMNDATATYPE COLUMNDBCOLUMN COLUMNQUERYSELECTION COLUMNTABLE COLUMNVALMSG DBCOLUMNDATANAME DBCOLUMNHANDLE EXCLUDECOLUMNS GETDATACOLUMNS GETFOREIGNVALUES GETQUERYPOSITION GETQUERYSORT GETQUERYSTRING GETQUERYWHERE GETTARGETPROCEDURE INDEXINFORMATION INSERTEXPRESSION NEWQUERYSTRING NEWQUERYVALIDATE NEWQUERYWHERE NEWWHERECLAUSE REFRESHROWIDENT REMOVEFOREIGNKEY REMOVEQUERYSELECTION RESOLVEBUFFER ROWIDWHERE ROWIDWHERECOLS SETQUERYPOSITION SETQUERYSORT SETQUERYSTRING SETQUERYWHERE WHERECLAUSEBUFFER GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataObject ContainerType PropertyDialog adm2/support/datad.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties AppService,ASInfo,ASUsePrompt,CacheDuration,CheckCurrentChanged,DestroyStateless,DisconnectAppServer,ServerOperatingMode,ShareData,UpdateFromSource,ForeignFields,ObjectName,OpenOnInit,PromptColumns,PromptOnDelete,RowsToBatch,RebuildOnRepos,ToggleDataTargets SupportedLinks Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext AutoCommit BLOBColumns BufferHandles CLOBColumns CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState CurrentRowid ROWID CurrentUpdateSource DataColumns DataHandle DataIsFetched DataModified DataQueryBrowsed DataQueryString FetchOnOpen FillBatchOnRepos FilterActive FilterAvailable FilterSource FilterWindow FirstRowNum ForeignFields ForeignValues IgnoreTreeViewFilter IndexInformation LargeColumns LastRowNum NavigationSource NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter OpenOnInit PrimarySDOSource PromptColumns PromptOnDelete QueryColumns QueryPosition QueryString RebuildOnRepos RowObject RowObjectState NoUpdates RowsToBatch Tables ToggleDataTargets TransferChildrenForAll UpdatableColumns UpdatableWhenNew UpdateSource AssignList AuditEnabled BaseQuery CalcFieldList CheckLastOnOpen DataColumnsByTable DBNames EntityFields FetchHasAudit FetchHasComment FetchAutoComment FirstResultRow KeyFields KeyTableId LastDBRowIdent LastResultRow NewBatchInfo NoLockReadOnlyTables PhysicalTables PositionForClient QueryHandle QueryRowIdent RequiredProperties SkipTransferDBRow TempTables UpdatableColumnsByTable UpdateFromSource RowObjUpd RowObjectTable RowObjUpdTable CheckCurrentChanged StatelessSavedProperties CheckCurrentChanged,RowObjectState,LastResultRow,FirstResultRow,QueryRowIdent DestroyStateless DisconnectAppServer ServerSubmitValidation DataFieldDefs "Util/xxx/dtables.i" QueryContainer QueryContext AsynchronousSDO DataLogicProcedure DataLogicObject DataReadHandler DataReadColumns DataReadBuffer DataDelimiter | DataReadFormat TrimNumeric IsRowObjectExternal IsRowObjUpdExternal ManualAddQueryWhere DynamicData LastCommitErrorType LastCommitErrorKeys RunDataLogicProxy SchemaLocation CacheDuration INTEGER ShareData ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService adm2/dataquery.p dataAvailable,confirmContinue,isUpdatePending,buildDataRequest adm2/query.p adm2/queryext.p cTable iTable cColumns cDataCols cUpdCols cCalcData cCalcUpd iNumData iNumUpd cBuffers cKeyFields cAssignList iAssigns iPos iEntry iCount cTables cTableAssign cDbEntry cField cKeyTable cQueryString FOR EACH Almacen NO-LOCK INDEXED-REPOSITION ,   hQuery cOpenQuery cDBNames cPhysicalTables cKeyTableEntityFields cKeyTableEntityValues cKeyTableEntityMnemonic cKeyTableEntityObjField cDBName cEntityFields lHasObjectField lHasAudit lHasComment lHasAutoComment iLookup iAlias  STATIC setDBNames OPEN QUERY Query-Main FOR EACH Almacen NO-LOCK INDEXED-REPOSITION.  FOR   PRESELECT  setOpenQuery 5 showMessage ; CodAlm Descripcion CodCia Query-Main INITPROPS piTableIndex lRet DELETERECORDSTATIC adm2/data.p adm2/dataext.p adm2/dataextcols.p adm2/dataextapi.p CodAlm Descripcion CodCia RowNum RowIdent RowMod RowIdentIdx RowUserProp ChangedFields cContainerType hRowObject hDataQuery cDataFieldDefs FOR EACH  setRowObjectTable setRowObjUpdTable getUpdatableColumns REMOVE Update-Target PUSHROWOBJUPDTABLE pcValType PUSHTABLEANDVALIDATE pcContext pcMessages pcUndoIds obtainContextForClient REMOTECOMMIT SERVERCOMMIT GETROWOBJUPDSTATIC pocMessages pocUndoIds BUFFERCOMMIT DISABLE_UI qDataQuery �  �0  �  �>      / �<   ��      0         pcServices      ��      T         pcServices  �   ��      x         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow  �   ��      �         piStartRow      ��              piStartRow  <  ��      ,        pcViewColList       ��      T       
 pcViewColList       ��      |        pcRelative  �  ��      �        pcSdoName       ��      �  �     
 pcSdoName       ��      �        plForwards      ��              pcContext       ��      0        plUpdate    `  ��      T        pcFieldList �  ��      x        pcFieldList     ��      �        pcFieldList �  ��      �        piocContext �  ��      �        piocContext   ��              piocContext 8  ��      ,        piocContext \  ��      P        piocContext �  ��      t        piocContext �  ��      �  �     
 piocContext     ��      �        piocContext     ��      �        pcState     ��               pcContext   0  ��      $        piStartRow  T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow      ��      �  �     
 phRowObjUpd     ��               pcProperties    T  ��      H        piStartRow  x  ��      l        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow  �  ��      �        piStartRow      ��      �  �     
 piStartRow  ,  ��               pcRowIdent      ��      D        pcRowIdent  t  ��      h        pcRowIdent  �  ��      �        pcRowIdent      ��      �        pcRowIdent  �  ��      �        pcValueList     ��      �        pcValueList 4  ��              pcPropertiesForServer       ��      L        pcPropertiesForServer   �  ��      |        pcFieldList �  ��      �        pcFieldList �  ��      �        pcFieldList     ��      �        pcFieldList   ��              pcWhere     ��      ,        pcWhere     ��      L        pcState     ��      l       
 phRowObjUpd �  ��      �       
 phRowObj    �  ��      �       
 phRowObj    �  ��      �        phRowObj        ��      �        phRowObj        ��       	        pioCancel       ��      D	        pcRelative      ��      h	       
 phFilterContainer       ��      �	       
 phRowObjUpd �	  ��      �	        pcRowIdent      ��      �	        pcRowIdent      ��       
       
 phAppService        ��      (
        pcMode  T
  ��      H
       
 phSource    x
  ��      l
        phSource        ��      �
       
 phSource    �
  ��      �
        pcText  �
  ��      �
        pcText      ��      �
        pcText     ��             
 phObject    D  ��      8       
 phObject        ��      \        phObject        ��      �        pcField     ��      �        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller      ��              phCaller        ��      0        phCaller    \  ��      T        pcMod   |  ��      t        pcMod       ��      �       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    (  ��               pdRow       ��      @        pdRow       ��      `       
 hTarget �  ��      �        pcMessage       ��      �        pcMessage       ��      �        plEnabled             �     cType       0     V   �                             getObjectType   O	  g	  i	  `        P  
   hReposBuffer    �        t  
   hPropTable  �        �  
   hBuffer           �  
   hTable  �  �     W   <          �                  adm-clone-props l  m  n  o  q  r  s  t  u  v  w  x  {  ~    �  �            P  
   hProc             p        pcProcName  �  �  	   X   <  X      �                  start-super-proc    �  �  �  �  �  �  �  �  �           �     cTable               iTable  <        0     cColumns    \        P     cDataCols   |        p     cUpdCols    �        �     cCalcData   �        �     cCalcUpd    �     	   �     iNumData    �     
   �     iNumUpd              cBuffers    8        ,     cKeyFields  X        L     cAssignList x        l     iAssigns    �        �     iPos    �        �     iEntry  �        �     iCount  �        �     cTables        �     cTableAssign    ,              cDbEntry    H        @     cField  h        \     cKeyTable   �        |     cQueryString    �        �  
   hQuery  �        �  
   hBuffer �        �     cOpenQuery          �     cDBNames    (             cPhysicalTables T        <     cKeyTableEntityFields   �        h     cKeyTableEntityValues   �        �     cKeyTableEntityMnemonic �         �     cKeyTableEntityObjField �     !   �     cDBName      "        cEntityFields   <     #   ,     lHasObjectField \     $   P     lHasAudit   |     %   p     lHasComment �     &   �     lHasAutoComment �     '   �     iLookup        (   �     iAlias  |    3   Y   �                            initProps   l  m  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �  �  *  /  1  2  3  6  8  :  ;  >  ?  E  S  �            �     lRet                      piTableIndex    �  h  *   Z   �  �      T                  deleteRecordStatic  �  �  �  �  �  �  �  �      !  "  >  ?  [  \  x  y  �  �  �  �  �  �  �  �  	  
  &  '  C  D  `  a  }  ~  �  �  �  �  �  �                 !       $  l     [             X                  pushRowObjUpdTable    �        �        pcValType                  $       (  �     \       p      �                  pushTableAndValidate    !  "  $  $                pcContext   <             $       `        T        pcMessages            x        pcUndoIds   �  �     ]              �                  remoteCommit    M  O  S  W  Y  �             $                       pcMessages            ,        pcUndoIds   �  x     ^       �      h                  serverCommit    �  �  8  �     _               �                  getRowObjUpdStatic  �  �  �         �        pocMessages                    pocUndoIds  �  T     `       �      D                  bufferCommit    �  �  �    �     a               �                  disable_UI  �     `   "  
     �      �!                      �  �  �     RowObject   P         X         d         l         t         �         �         �         CodAlm  Descripcion CodCia  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp     �  �  	   RowObjUpd   (         0         <         D         L         X         `         l         x         CodAlm  Descripcion CodCia  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp ChangedFields   �          �  
   appSrvUtils �        �     s-codcia    �       �     xiRocketIndexLimit            
   gshAstraAppserver   @        ,  
   gshSessionManager   d        T  
   gshRIManager    �        x  
   gshSecurityManager  �  	 	     �  
   gshProfileManager   �  
 
     �  
   gshRepositoryManager            �  
   gshTranslationManager   0           
   gshWebManager   T        D     gscSessionId    x        h     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager           �     gsdTempUniqueID (             gsdUserObj  P        <     gsdRenderTypeObj    x        d     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf            �     glADMLoadFromRepos                glADMOk <        0   
   ghContainer \     	   P      cObjectName x     
   p      iStart  �        �      cAppService �        �      cASDivision �        �      cServerOperatingMode    !       �      cContainerType  ,!       !     cQueryString    L!       @!  
   hRowObject  l!       `!  
   hDataQuery  �!       �!     cColumns             �!     cDataFieldDefs  �!       �!  Almacen �!    X  �!  RowObject         X  �!  RowObjUpd            9   �   �   �   �            7  C  D  E  G  I  J  K  O  P  S  T  U  V  X  Z  \  ^  _  `  c  e  f  h  i  j  k  l  r  t  z  |  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  @
  A
  C
  D
  E
  F
  G
  H
  J
  K
  L
  M
  N
  O
  P
  Q
  R
  S
  T
  U
  V
  W
  X
  Y
  Z
  [
  \
  ]
  ^
  _
  `
  a
  b
  d
  e
  f
  g
  h
  i
  j
  k
  l
  m
  n
  o
  p
  q
  r
  s
  t
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
  �
                     	  
      }  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  ,  �  �  �  �  �  �  �  �  �      /  N  P  e  �         0  1  2  5  6  7  >  ?  \  p  �  "  #  /  S  �  �  �  �  �  @  �  �  �  �  �  �  �  2  L  V  p  z  �  �  �  �  �  �  �      &  @  b  m  n      ��  C:\Progress\OpenEdge\src\adm2\data.i (&  �) . %C:\Progress\OpenEdge\src\adm2\custom\datacustom.i    X&  �� - C:\Progress\OpenEdge\src\adm2\robjflds.i �&  � , .\Util\xxx\dtables.i �&  �:  C:\Progress\OpenEdge\src\adm2\query.i    �&  z + C:\Progress\OpenEdge\src\adm2\delrecst.i  '  `W * C:\Progress\OpenEdge\src\adm2\tblprep.i  T'  F� ) C:\Progress\OpenEdge\gui\fnarg   �'   ( %C:\Progress\OpenEdge\src\adm2\custom\querycustom.i   �'  �   C:\Progress\OpenEdge\src\adm2\dataquery.i    �'  �Z ' %C:\Progress\OpenEdge\src\adm2\custom\dataquerycustom.i   ,(  �< ! C:\Progress\OpenEdge\src\adm2\appserver.i    p(  �� & %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �(  I� " C:\Progress\OpenEdge\src\adm2\smart.i    �(  Ds % C:\Progress\OpenEdge\gui\fn   )  tw $ %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   H)  Q. # C:\Progress\OpenEdge\gui\set �)  �>  C:\Progress\OpenEdge\src\adm2\dataprop.i �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\datapropcustom.i    �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataprtocustom.i    (*  �K  C:\Progress\OpenEdge\src\adm2\qryprop.i  l*  -�  %C:\Progress\OpenEdge\src\adm2\custom\qrypropcustom.i �*  ��  %C:\Progress\OpenEdge\src\adm2\custom\qryprtocustom.i �*   	 C:\Progress\OpenEdge\src\adm2\dataqueryprop.i     +  �d  %C:\Progress\OpenEdge\src\adm2\custom\dataquerypropcustom.i   \+  ��  %C:\Progress\OpenEdge\src\adm2\custom\dataqueryprtocustom.i   �+  �l  C:\Progress\OpenEdge\src\adm2\appsprop.i �+  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i     ,  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    d,  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �,  �j  C:\Progress\OpenEdge\gui\get �,  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    -  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    H-  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �-  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �-  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �-  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   4.  �  C:\Progress\OpenEdge\src\adm2\appsprto.i x.  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �.  ��  C:\Progress\OpenEdge\src\adm2\dataqueryprto.i    �.  ª 
 %C:\Progress\OpenEdge\src\adm2\custom\dataquerydefscustom.i   0/  ��  C:\Progress\OpenEdge\src\adm2\qryprto.i  x/  �  %C:\Progress\OpenEdge\src\adm2\custom\querydefscustom.i   �/  �`  C:\Progress\OpenEdge\src\adm2\dataprto.i �/  �  %C:\Progress\OpenEdge\src\adm2\custom\datadefscustom.i    $0  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   h0  �    C:\newsie\on_in_co\Util\xxx\dtables.w        �   �      �0  [  V     �0     T  %   �0  �   �     1     v  .   1  �   l     $1     M     41  �   J     D1     (  #   T1  �   &     d1       #   t1  �        �1     �  #   �1  �   �     �1     �  #   �1  �   �     �1     �  #   �1  �   �     �1     r  #   �1  �   p     2     N  #   2  �   L     $2     *  #   42  �        D2       -   T2  �        d2     �  ,   t2  k   �     �2  �  �     �2     �  +   �2  �  �     �2     �  +   �2  �  }     �2     c  +   �2  �  `     �2     F  +   3  �  C     3     )  +   $3  �  &     43       +   D3  �  	     T3     �  +   d3  �  �     t3     �  +   �3  �  �     �3     �  +   �3  �  �     �3     �  +   �3  �  �     �3     {  +   �3  �  x     �3     ^  +   4  �  [     4     A  +   $4  �  >     44     $  +   D4  �  !     T4       +   d4  �       t4     �  +   �4  �  �     �4     �  +   �4  �  �     �4     �  +   �4  �  �     �4     n  #   �4  �  m     �4     K  #   5  k  &     5       #   $5  j       45     �  #   D5  i  �     T5     �  #   d5  _  �     t5     �  *   �5  ^  �     �5     g  *   �5  ]  f     �5     @  *   �5  \  ?     �5       *   �5  [       �5     �  *   6  Z  �     6     �  *   $6  Y  �     46     �  *   D6  X  �     T6     }  *   d6  W  |     t6     V  *   �6  V  U     �6     /  *   �6  U  .     �6       *   �6  T       �6     �  *   �6  S  �     �6     �  *   7  R  �     7     �  *   $7  Q  �     47     l  *   D7  P  k     T7     E  *   d7  O  D     t7       *   �7  N       �7     �  *   �7  @  �     �7     �  #   �7  	  �     �7     �  )   �7  �   }     �7     [  #   8  �   Z     8     8  #   $8  �   7     48       #   D8  �        T8     �  #   d8  �   �     t8     �  #   �8  �   �     �8     �  #   �8  �   <     �8     �  (   �8  g   �     �8  a   �      �8     o  '   �8  _   m      9     K  #   9  ]   I      $9     '  #   49  I         D9  �   
  !   T9     �  &   d9  �   �  !   t9     �  #   �9  �   �  !   �9     h  #   �9  �   f  !   �9     D  #   �9  g   *  !   �9          �9  O   �  !   �9  �   }  "   :     {  %   :  �   K  "   $:     �  $   4:  �   �  "   D:     �  #   T:  �   �  "   d:     �  #   t:  �   �  "   �:     �  #   �:  �     "   �:     ]  #   �:  �   I  "   �:     '  #   �:  }     "   �:     �  #   �:     }  "   ;     /  !   ;     �      $;     �     4;     5     D;  �   ,     T;  O        d;          t;     �     �;  �   �     �;  �   }     �;  O   o     �;     ^     �;          �;  y   �
     �;  �   �
  	   �;  G   �
     <     �
     <     x
     $<  c   
  	   4<  x   
     D<  M   �	     T<     �	     d<     �	     t<  a   �	     �<  �  f	     �<     G	     �<  �  	     �<  O   	     �<     �     �<     �     �<  �   �     �<     �     =     �     =  x   �     $=     �     4=     b     D=     ^     T=     J     d=     1     t=  Q   !     �=     �     �=     �     �=     {     �=     a     �=  ]   [  	   �=     Q     �=     	  	   �=     �  
   >     �  	   >  Z   �     $>     �     4>     �     D>     �     T>     �     d>  c   e     t>     C     �>     �      �>     �      �>     �      �>     �      �>     !       �>           