	��V��*Ix2    �              �                                 �� 32780106utf-8 MAIN O:\OpenEdge\on_in_co\APLIC\btablewi.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewObject,, PROCEDURE updateTitle,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateRecord,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE setDown,,INPUT piNumDown INTEGER PROCEDURE searchTrigger,, PROCEDURE rowDisplay,, PROCEDURE resizeObject,,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL PROCEDURE resizeBrowse,,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL PROCEDURE resetRecord,, PROCEDURE refreshBrowse,, PROCEDURE offHome,, PROCEDURE offEnd,, PROCEDURE initializeObject,, PROCEDURE filterActive,,INPUT plActive LOGICAL PROCEDURE fetchDataSet,,INPUT pcState CHARACTER PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFields CHARACTER PROCEDURE destroyObject,, PROCEDURE deleteRecord,, PROCEDURE deleteComplete,, PROCEDURE defaultAction,, PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE calcWidth,, PROCEDURE assignMaxGuess,,INPUT piMaxGuess INTEGER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE applyCellEntry,,INPUT pcCellName CHARACTER PROCEDURE addRecord,, FUNCTION getRowObject,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION getObjectType,character, FUNCTION stripCalcs,CHARACTER,INPUT cClause CHARACTER FUNCTION setVisibleRowReset,LOGICAL,INPUT plReset LOGICAL FUNCTION setVisibleRowids,LOGICAL,INPUT pcRowids CHARACTER FUNCTION setSearchField,LOGICAL,INPUT pcField CHARACTER FUNCTION setScrollRemote,LOGICAL,INPUT plScrollRemote LOGICAL FUNCTION setQueryRowObject,LOGICAL,INPUT phQueryRowObject HANDLE FUNCTION setNumDown,LOGICAL,INPUT piNumDown INTEGER FUNCTION setMaxWidth,LOGICAL,INPUT pdMaxWidth DECIMAL FUNCTION setDataModified,LOGICAL,INPUT lModified LOGICAL FUNCTION setCalcWidth,LOGICAL,INPUT plCalcWidth LOGICAL FUNCTION setApplyExitOnAction,LOGICAL,INPUT plApply LOGICAL FUNCTION setApplyActionOnExit,LOGICAL,INPUT plApply LOGICAL FUNCTION setActionEvent,LOGICAL,INPUT pcEvent CHARACTER FUNCTION rowVisible,CHARACTER,INPUT pcRowids CHARACTER,INPUT phQryBuffer HANDLE FUNCTION getVisibleRowReset,LOGICAL, FUNCTION getVisibleRowids,CHARACTER, FUNCTION getTargetProcedure,HANDLE, FUNCTION getSearchField,CHARACTER, FUNCTION getScrollRemote,LOGICAL, FUNCTION getQueryRowObject,HANDLE, FUNCTION getNumDown,INTEGER, FUNCTION getMaxWidth,DECIMAL, FUNCTION getDataSignature,CHARACTER, FUNCTION getCalcWidth,LOGICAL, FUNCTION getBrowseHandle,HANDLE, FUNCTION getApplyExitOnAction,LOGICAL, FUNCTION getApplyActionOnExit,LOGICAL, FUNCTION getActionEvent,CHARACTER, FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER       d              d             )� d   �              �i              X+    +   <p �  N   �t `  O   <x �   S   0y t  b           �z �  t| x  ? �} =  iSO8859-1                                                                        �   �    X                                     �                   ��                  �    �   &   ��             ��  �   ,      8                                                         PROGRESS                         �          �       �  X  �       �  �W      h         �             \                �     �      �  
    
                  l  4             �                                                                                          �          
  �  �      ,  
    
                    �             �                                                                                          �          
  \  �      �  
    
                  �  �             H                                                                                          �          
    �      �  
    
                  p  8             �                                                                                          �          
  �  	      0  
    
                    �             �                                                                                          	          
  `  	      �  
    
                  �  �             L                                                                                          	          
    ,	      �  
    
                  t  <             �                                                                                          ,	          
  �  B	      4  
    
                     �  	           �                                                                                          B	          
  d  P	      �                         �  �  
           P                                                                                          P	              ]	      �                        x  @             �                                                                                          ]	            �  k	      8  
    
                  $  �             �                                                                                          k	          
  h	  y	      �  
    
                  �  �	             T	                                                                                          y	          
  
  �	      �	  
    
                  |	  D
              
                                                                                          �	          
  �
  �	      <
                        (
  �
             �
                                                                                          �	            l  �	      �
                        �
  �             X                                                                                          �	              �	      �                        �  H                                                                                                       �	                �	      @                        ,                 �                                                                                          �	                          P�                                               T�          �  �  ` �,            
             
             
                                         
                                                                                                                                           
                                         
             
                                                        `   p   �   �   �   �   �   �   �   �           0  @  P  `  p  �  �  �  �  �      `   p   �   �   �   �   �   �   �   �          0  @  P  `  p  �  �  �  �  �                                                                                                                                     	                                 �  �  �  �  �          �             �  �  �  �  �          �                    ,             0             H  P  \  d                             h  t  |  �                              �  �  �  �                             �  �  �  �                             �  �  �  �                                                                          CodCia  999 Cia Cia 0   C�digo de Compa�ia  CodAlm  x(3)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ���	������               �        �        �                �     i     i     i     	 	 	    �  �  �  �  �  �  �  �    ��                                               �                             �          ����                            �    t�  2                 o�    undefined                                                               �       x�  �   l   ��                        �����               Ȍ�                    O   ����    e�          O   ����    R�          O   ����    ��      
               assignFocusedWidget         �       �             LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �             P           LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList   0      �      �    *       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget �            H    @       LOGICAL,INPUT pcNameList CHARACTER  clearWidget (      l      �    L       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  x      �      �    X       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �      0      `    k       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton   @      �      �    y       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    �      �      (    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue          L      �  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    d      �      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      (      T   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget 4      x      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    �      �           �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget           D      t    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  T      �      �   
 �       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    �      �          
      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �      8      l          LONGCHAR,INPUT pcName CHARACTER widgetIsBlank   L      �      �    +      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused �      �          9      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      0      d    I      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    D      �      �    Z      LOGICAL,INPUT pcName CHARACTER  widgetValue �      �      	    g      CHARACTER,INPUT pcName CHARACTER    widgetValueList �      (	      X	    s      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �                �              �              � ߱            Z   �����	   ��	                     ��    @  0
  �
             4   ����                 �
                      ��                  @  D                  @��                       @  @
     	  A  �
                                        3   ����4       O   C  ��  ��  @   addRecord                               �  �      ��                  �     �              \��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            applyCellEntry                              �  �      ��                      �              T��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            applyEntry                              �  �      ��                      �              p��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            assignMaxGuess                                 �      ��                  
                  ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  0           ��                            ����                            calcWidth                               (        ��                      @              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                ,        ��                      D              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                              ,        ��                      D              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            defaultAction                               0        ��                      H              X��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deleteComplete                              4        ��                      L              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deleteRecord                                8         ��                      P              $�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               <  $      ��                     !  T              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                               @  (      ��                  #  %  X              $O�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p           ��                            ����                            displayFields                               l  T      ��                  '  )  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            enableFields                                �  �      ��                  +  ,  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchDataSet                                �  �      ��                  .  0  �              �"�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            filterActive                                �  �      ��                  2  4  �              �2�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  6  7                <7�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            offEnd                              �  �      ��                  9  :                �7�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            offHome                             �  �      ��                  <  =                K�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refreshBrowse                               �  �      ��                  ?  @                 �K�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            resetRecord                             �   �       ��                  B  C  !              @L�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            resizeBrowse                                �!  �!      ��                  E  H  "              �&�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   \"             ("               ��                  P"           ��                            ����                            resizeObject                                L#  4#      ��                  J  M  d#              0G�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �#             |#               ��                  �#           ��                            ����                            rowDisplay                              �$  �$      ��                  O  P  �$              �*�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            searchTrigger                               �%  �%      ��                  R  S  �%              �+�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            setDown                             �&  �&      ��                  U  W  �&              x�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �&           ��                            ����                            toolbar                             �'  �'      ��                  Y  [  �'              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �'           ��                            ����                            updateRecord                                �(  �(      ��                  ]  ^  )              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �)  �)      ��                  `  b  *              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  *           ��                            ����                            updateTitle                             +  �*      ��                  d  e  ,+              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewObject                              ,  �+      ��                  g  h  ,,              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            colValues   8	      �,      �,   	 N      CHARACTER,INPUT pcViewColList CHARACTER getActionEvent  �,      �,      -    X      CHARACTER,  getApplyActionOnExit    �,      -      L-    g      LOGICAL,    getApplyExitOnAction    ,-      X-      �-    |      LOGICAL,    getBrowseHandle p-      �-      �-    �      HANDLE, getCalcWidth    �-      �-      .    �      LOGICAL,    getDataSignature    �-      .      D.    �      CHARACTER,  getMaxWidth $.      P.      |.    �      DECIMAL,    getNumDown  \.      �.      �.    
 �      INTEGER,    getQueryRowObject   �.      �.      �.  !  �      HANDLE, getScrollRemote �.      �.      ,/  "  �      LOGICAL,    getSearchField  /      8/      h/  #  �      CHARACTER,  getTargetProcedure  H/      t/      �/  $        HANDLE, getVisibleRowids    �/      �/      �/  %        CHARACTER,  getVisibleRowReset  �/      �/      $0  &  +      LOGICAL,    rowVisible  0      00      \0  ' 
 >      CHARACTER,INPUT pcRowids CHARACTER,INPUT phQryBuffer HANDLE setActionEvent  <0      �0      �0  (  I      LOGICAL,INPUT pcEvent CHARACTER setApplyActionOnExit    �0      �0       1  )  X      LOGICAL,INPUT plApply LOGICAL   setApplyExitOnAction     1      @1      x1  *  m      LOGICAL,INPUT plApply LOGICAL   setCalcWidth    X1      �1      �1  +  �      LOGICAL,INPUT plCalcWidth LOGICAL   setDataModified �1      �1      2  ,  �      LOGICAL,INPUT lModified LOGICAL setMaxWidth �1      <2      h2  -  �      LOGICAL,INPUT pdMaxWidth DECIMAL    setNumDown  H2      �2      �2  . 
 �      LOGICAL,INPUT piNumDown INTEGER setQueryRowObject   �2      �2      3  /  �      LOGICAL,INPUT phQueryRowObject HANDLE   setScrollRemote �2      43      d3  0  �      LOGICAL,INPUT plScrollRemote LOGICAL    setSearchField  D3      �3      �3  1  �      LOGICAL,INPUT pcField CHARACTER setVisibleRowids    �3      �3      4  2  �      LOGICAL,INPUT pcRowids CHARACTER    setVisibleRowReset  �3      44      h4  3  �      LOGICAL,INPUT plReset LOGICAL   stripCalcs  H4      �4      �4  4 
       CHARACTER,INPUT cClause CHARACTER   getObjectType   �4      �4      5  5        CHARACTER,  addRecord                               �5  �5      ��                  ]  ^  �5              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �6  �6      ��                  `  a  �6              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                              �7  �7      ��                  c  f  �7              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��   8             �7               ��                  8           ��                            ����                            confirmContinue                              9  �8      ��                  h  j  9              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  09           ��                            ����                            confirmDelete                               ,:  :      ��                  l  n  D:              <��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \:           ��                            ����                            confirmExit                             T;  <;      ��                  p  r  l;              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �;           ��                            ����                            copyRecord                              |<  d<      ��                  t  u  �<              �϶                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               �=  h=      ��                  w  y  �=              �Ҷ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �=           ��                            ����                            deleteRecord                                �>  �>      ��                  {  |  �>              �ֶ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �?  �?      ��                  ~    �?              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �@  �@      ��                  �  �  �@              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   $A             �@               ��                  A           ��                            ����                            queryPosition                               B  �A      ��                  �  �  ,B              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  DB           ��                            ����                            resetRecord                             <C  $C      ��                  �  �  TC              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                               LD  4D      ��                  �  �  dD              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |D           ��                            ����                            updateMode                              tE  \E      ��                  �  �  �E              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �E           ��                            ����                            updateRecord                                �F  �F      ��                  �  �  �F              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �G  �G      ��                  �  �  �G              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �G           ��                            ����                            updateTitle                             �H  �H      ��                  �  �  �H              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              �I  �I      ��                  �  �  �I              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �I           ��                            ����                            getCreateHandles    �4      dJ      �J  6  $      CHARACTER,  getDataModified xJ      �J      �J  7  5      LOGICAL,    getDisplayedFields  �J      �J      K  8  E      CHARACTER,  getDisplayedTables  �J       K      TK  9  X      CHARACTER,  getEnabledFields    4K      `K      �K  :  k      CHARACTER,  getEnabledHandles   tK      �K      �K  ;  |      CHARACTER,  getFieldHandles �K      �K      L  <  �      CHARACTER,  getFieldsEnabled    �K      L      PL  =  �      LOGICAL,    getGroupAssignSource    0L      \L      �L  >  �      HANDLE, getGroupAssignSourceEvents  tL      �L      �L  ?  �      CHARACTER,  getGroupAssignTarget    �L      �L      M  @  �      CHARACTER,  getGroupAssignTargetEvents  �L      (M      dM  A  �      CHARACTER,  getNewRecord    DM      pM      �M  B        CHARACTER,  getObjectParent �M      �M      �M  C        HANDLE, getRecordState  �M      �M      N  D  ,      CHARACTER,  getRowIdent �M       N      LN  E  ;      CHARACTER,  getTableIOSource    ,N      XN      �N  F  G      HANDLE, getTableIOSourceEvents  lN      �N      �N  G  X      CHARACTER,  getUpdateTarget �N      �N      O  H  o      CHARACTER,  getUpdateTargetNames    �N      O      LO  I        CHARACTER,  getWindowTitleField ,O      XO      �O  J  �      CHARACTER,  okToContinue    lO      �O      �O  K  �      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    �O      �O       P  L  �      LOGICAL,INPUT pcContainerMode CHARACTER setDisplayedFields   P      HP      |P  M  �      LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    \P      �P      �P  N  �      LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    �P      �P      0Q  O  �      LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  Q      PQ      �Q  P  �      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    lQ      �Q      �Q  Q        LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  �Q      R      HR  R  /      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    (R      lR      �R  S  J      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent �R      �R       S  T  _      LOGICAL,INPUT phParent HANDLE   setSaveSource   �R       S      PS  U  o      LOGICAL,INPUT plSave LOGICAL    setTableIOSource    0S      pS      �S  V  }      LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  �S      �S      �S  W  �      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget �S       T      PT  X  �      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    0T      tT      �T  Y  �      LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField �T      �T      U  Z  �      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    �T      4U      hU  [  �      CHARACTER,  applyLayout                             V  �U      ��                  �  �  V              H��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               W  �V      ��                  �  �   W              蟹                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                X  �W      ��                  �  �  $X              @��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                Y  �X      ��                  �  �  ,Y              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               Z   Z      ��                  �  �  0Z              H��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  HZ           ��                            ����                            getAllFieldHandles  HU      �Z      �Z  \  �      CHARACTER,  getAllFieldNames    �Z      �Z      $[  ]        CHARACTER,  getCol  [      0[      X[  ^        DECIMAL,    getDefaultLayout    8[      d[      �[  _        CHARACTER,  getDisableOnInit    x[      �[      �[  `  +      LOGICAL,    getEnabledObjFlds   �[      �[      \  a  <      CHARACTER,  getEnabledObjHdls   �[      $\      X\  b  N      CHARACTER,  getHeight   8\      d\      �\  c 	 `      DECIMAL,    getHideOnInit   p\      �\      �\  d  j      LOGICAL,    getLayoutOptions    �\      �\      ]  e  x      CHARACTER,  getLayoutVariable   �\      ]      L]  f  �      CHARACTER,  getObjectEnabled    ,]      X]      �]  g  �      LOGICAL,    getObjectLayout l]      �]      �]  h  �      CHARACTER,  getRow  �]      �]      �]  i  �      DECIMAL,    getWidth    �]      ^      4^  j  �      DECIMAL,    getResizeHorizontal ^      @^      t^  k  �      LOGICAL,    getResizeVertical   T^      �^      �^  l  �      LOGICAL,    setAllFieldHandles  �^      �^      �^  m  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �^      _      H_  n        LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    (_      h_      �_  o        LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    |_      �_      �_  p  '      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �_      `      D`  q  8      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    $`      d`      �`  r  F      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout x`      �`      �`  s  W      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �`      a      Da  t  g      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   $a      pa      �a  u  {      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated �a      �a       b  v  �      LOGICAL,    getObjectSecured    �a      b      @b  w  �      LOGICAL,    createUiEvents   b      Lb      |b  x  �      LOGICAL,    addLink                             c  �b      ��                  �  �  ,c              $�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  xc             Dc  
             ��   �c             lc               �� 
                 �c  
         ��                            ����                            addMessage                              �d  td      ��                  �  �  �d              <�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �d             �d               ��   e             �d               ��                  e           ��                            ����                            adjustTabOrder                              f  �e      ��                  �  �   f              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  lf             8f  
             �� 
  �f             `f  
             ��                  �f           ��                            ����                            applyEntry                              �g  hg      ��                  �  �  �g              `��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �g           ��                            ����                            changeCursor                                �h  �h      ��                  �  �  �h              p�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �h           ��                            ����                            createControls                              �i  �i      ��                  �  �  �i              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �j  �j      ��                  �  �  �j              �	�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �k  �k      ��                  �  �  �k              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �l  �l      ��                  �  �  m              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �m  �m      ��                  �  �  n              8�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �n  �n      ��                  �  �  o              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �o  �o      ��                  �  �  p              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �p  �p      ��                  �  �  q               �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `q             ,q  
             ��   �q             Tq               ��   �q             |q               ��                  �q           ��                            ����                            modifyUserLinks                             �r  �r      ��                  �  �  �r              �"�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   s             �r               ��   ,s             �r               �� 
                  s  
         ��                            ����                            removeAllLinks                              t  t      ��                  �  �  4t              X.�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              u  u      ��                  �  �  4u              X1�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  �u             Lu  
             ��   �u             tu               �� 
                 �u  
         ��                            ����                            repositionObject                                �v  �v      ��                  �  �  �v              �8�                    O   ����    e�          O   ����    R�          O   ����    ��            ��    w             �v               ��                  �v           ��                            ����                            returnFocus                             �w  �w      ��                  �  �  x              �>�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 x  
         ��                            ����                            showMessageProcedure                                 y  y      ��                  �  �  8y              �B�                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �y             Py               ��                  xy           ��                            ����                            toggleData                              pz  Xz      ��                  �    �z              I�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �z           ��                            ����                            viewObject                              �{  �{      ��                      �{              �I�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  \b      |      4|  y 
 
      LOGICAL,    assignLinkProperty  |      @|      t|  z  
      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   T|      �|      �|  {  -
      CHARACTER,  getChildDataKey �|      }      8}  |  ;
      CHARACTER,  getContainerHandle  }      D}      x}  }  K
      HANDLE, getContainerHidden  X}      �}      �}  ~  ^
      LOGICAL,    getContainerSource  �}      �}      �}    q
      HANDLE, getContainerSourceEvents    �}      �}      8~  �  �
      CHARACTER,  getContainerType    ~      D~      x~  �  �
      CHARACTER,  getDataLinksEnabled X~      �~      �~  �  �
      LOGICAL,    getDataSource   �~      �~      �~  �  �
      HANDLE, getDataSourceEvents �~      �~      0  �  �
      CHARACTER,  getDataSourceNames        <      p  �  �
      CHARACTER,  getDataTarget   P      |      �  �  �
      CHARACTER,  getDataTargetEvents �      �      �  �        CHARACTER,  getDBAware  �      �      $�  � 
       LOGICAL,    getDesignDataObject �      0�      d�  �  $      CHARACTER,  getDynamicObject    D�      p�      ��  �  8      LOGICAL,    getInstanceProperties   ��      ��      �  �  I      CHARACTER,  getLogicalObjectName    Ȁ      �      ,�  �  _      CHARACTER,  getLogicalVersion   �      8�      l�  �  t      CHARACTER,  getObjectHidden L�      x�      ��  �  �      LOGICAL,    getObjectInitialized    ��      ��      �  �  �      LOGICAL,    getObjectName   ́      ��      (�  �  �      CHARACTER,  getObjectPage   �      4�      d�  �  �      INTEGER,    getObjectVersion    D�      p�      ��  �  �      CHARACTER,  getObjectVersionNumber  ��      ��      �  �  �      CHARACTER,  getParentDataKey    Ȃ      �      (�  �  �      CHARACTER,  getPassThroughLinks �      4�      h�  �         CHARACTER,  getPhysicalObjectName   H�      t�      ��  �        CHARACTER,  getPhysicalVersion  ��      ��      �  �  *      CHARACTER,  getPropertyDialog   ̃      ��      ,�  �  =      CHARACTER,  getQueryObject  �      8�      h�  �  O      LOGICAL,    getRunAttribute H�      t�      ��  �  ^      CHARACTER,  getSupportedLinks   ��      ��      �  �  n      CHARACTER,  getTranslatableProperties   Ą      ��      ,�  �  �      CHARACTER,  getUIBMode  �      8�      d�  � 
 �      CHARACTER,  getUserProperty D�      p�      ��  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    ��      ȅ       �  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ��      (�      T�  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    4�      x�      ��  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry ��      �      �  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   ��      |�      ��  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    ��      Ї       �  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ��      (�      X�  �  
      CHARACTER,  setChildDataKey 8�      d�      ��  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  t�      ��      ��  �  )      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  Ј      �      D�  �  <      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    $�      d�      ��  �  O      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled ��      ĉ      ��  �  h      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ؉       �      P�  �  |      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents 0�      p�      ��  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  ��      ̊       �  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ��      (�      X�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents 8�      |�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  ��      ԋ       �  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ��       �      T�  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    4�      |�      ��  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   ��      ̌      �  �        LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   �      (�      \�  �        LOGICAL,INPUT cVersion CHARACTER    setObjectName   <�      ��      ��  �  +      LOGICAL,INPUT pcName CHARACTER  setObjectVersion    ��      Ѝ      �  �  9      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    �      ,�      `�  �  J      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks @�      ��      ��  �  [      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   ��      ܎      �  �  o      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  �      4�      h�  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute H�      ��      ��  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   ��      �      �  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      <�      x�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  X�      ��      Ȑ  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      �      �  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      X�      ��  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   d�      ��      ԑ  � 	 �      CHARACTER,INPUT pcName CHARACTER    ̔      �  ��      T       4   ����T                 ��                      ��                    H                  ��                         $�          ��  8�      d       4   ����d                 H�                      ��                    G                  ���                         ̒  H�    4  d�  ��      x       4   ����x                 �                      ��                  @  B                  ��                       @  t�         A                                  T     
                    � ߱        t�  $  D  �  ���                           $  F  ��  ���                       �                         � ߱        ؛    L  �  d�      �      4   �����                t�                      ��                  M  	                  Ȅ�                       M  ��  ��  o   P      ,                                  �  $   Q  ԕ  ���                       $  @                       � ߱        �  �   R  D      (�  �   S  �      <�  �   U  ,      P�  �   W  �      d�  �   Y        x�  �   [  �      ��  �   \        ��  �   ]  @      ��  �   `  �      Ȗ  �   b  (      ܖ  �   c  �      �  �   e         �  �   f  �      �  �   g  �      ,�  �   h  T      @�  �   i  �      T�  �   o  	      h�  �   q  x	      |�  �   w  �	      ��  �   y  (
      ��  �   {  �
      ��  �   |        ̗  �   �  �      ��  �   �        ��  �   �  �      �  �   �  �      �  �   �  l      0�  �   �  �      D�  �   �        X�  �   �  X      l�  �   �  �      ��  �   �        ��  �   �  D      ��  �   �  �      ��  �   �  �      И  �   �  8      �  �   �  t      ��  �   �  �      �  �   �  �       �  �   �  (      4�  �   �  d      H�  �   �  �      \�  �   �  �      p�  �   �            �   �  T                      ��          �  �      ��                  8	  f	   �              ���                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                @                     P                         � ߱        Ț  $ L	  8�  ���                           O   d	  ��  ��  �               4�          $�  ,�    �                                             ��                            ����                                �4      ��      ��     M     <�                      5 8�                       ��    �	  ��  p�      �      4   �����                ��                      ��                  �	  
                  ���                       �	  �  ��  �   �	  �      ��  �   �	  p      ��  �   �	  �      М  �   �	  `      �  �   �	  �      ��  �   �	  H      �  �   �	  �       �  �   �	  8      4�  �   �	  �      H�  �   �	         \�  �   �	  �      p�  �   �	        ��  �   �	  �      ��  �   �	         ��  �   �	  |      ��  �   �	  �      ԝ  �   �	  t      �  �   �	  �      ��  �   �	  l      �  �   �	  �      $�  �   �	  d      8�  �   �	  �      L�  �   �	  \      `�  �   �	  �      t�  �   �	  T       ��  �   �	  �       ��  �   �	  L!          �   �	  �!      ġ    %
  ̞  H�      0"      4   ����0"                X�                      ��                  &
  �
                  ���                       &
  ܞ  l�  �   (
  �"      ��  �   )
  #      ��  �   *
  �#      ��  �   +
  �#      ��  �   1
  �$      П  �   2
  %      �  �   3
  x%      ��  �   4
  �%      �  �   5
  h&       �  �   6
  �&      4�  �   7
  X'      H�  �   8
  �'      \�  �   9
  (      p�  �   ;
  �(      ��  �   <
  �(      ��  �   =
  l)      ��  �   >
  �)      ��  �   ?
  T*      Ԡ  �   @
  �*      �  �   A
  <+      ��  �   B
  �+      �  �   C
  ,,      $�  �   D
  �,      8�  �   E
  -      L�  �   F
  X-      `�  �   H
  �-      t�  �   I
  @.      ��  �   K
  �.      ��  �   L
  0/      ��  �   M
  �/          �   N
   0      �    �
  �  \�      P0      4   ����P0                l�                      ��                  �
  m                  ��                       �
  �  ��  �   �
  �0      ��  �   �
  ,1      ��  �   �
  h1      ��  �   �
  �1      Т  �   �
  `2      �  �   �
  �2      ��  �   �
  P3      �  �   �
  �3       �  �   �
  @4      4�  �   �
  |4      H�  �   �
  �4      \�  �   �
  �4      p�  �   �
  05      ��  �   �
  l5      ��  �   �
  �5      ��  �   �
  �5      ��  �   �
   6      ԣ  �   �
  �6      �  �   �
  7      ��  �   �
  �7      �  �   �
  8      $�  �   �
  �8      8�  �   �
  �8      L�  �   �
  �8      `�  �   �
  89      t�  �   �
  t9      ��  �   �
  �9      ��  �   �
  ,:      ��  �   �
  h:      Ĥ  �   �
  �:      ؤ  �   �
  �:      �  �   �
  ;       �  �   �
  X;      �  �   �
  �;      (�  �   �
  �;      <�  �   �
  <      P�  �   �
  H<      d�  �   �
  �<      x�  �   �
  �<      ��  �   �
  �<      ��  �   �
  8=      ��  �   �
  t=      ȥ  �   �
  �=      ܥ  �   �
  �=      �  �   �
  (>          �   �
  d>      getRowObject    l�  $  }  @�  ���                       �>     
                    � ߱        �    �  ��  ��      �>      4   �����>      /   �  Ħ     Ԧ                          3   �����>            ��                      3   ����?  X�    �   �  ��  ��  4?      4   ����4?  	              ��                      ��             	     �  E                  ú                       �  0�  ��  �   �  �?      �  $  �  �  ���                       �?     
                    � ߱        ,�  �   �  �?      ��  $   �  X�  ���                       @  @         �?              � ߱        @�  $  �  ��  ���                       \@                         � ߱        A     
                �A                     �B  @        
 �B              � ߱        Щ  V   �  ܨ  ���                        �B                     (C       	       	       dC                         � ߱        `�  $  �  l�  ���                       $D     
                �D                     �E  @        
 �E              � ߱        �  V     ��  ���                        �E     
                xF                     �G  @        
 �G              � ߱            V   )  ��  ���                        
              P�                      ��             
     G  �                  �ĺ                       G  �  �G     
                XH                     �I  @        
 hI          J  @        
 �I          tJ  @        
 4J          �J  @        
 �J              � ߱            V   \  ��  ���                        adm-clone-props |�  |�              �     N     `                          \  ^                     start-super-proc    ��  �  �           �     O                                                       �    �  t�  ��      `N      4   ����`N      /   �  ��     ��                          3   ����pN            �                      3   �����N  ��  $    �  ���                       �N       
       
           � ߱        �N     
                TO                     �P  @        
 dP              � ߱        خ  V     H�  ���                        ��    �  ��  p�      �P      4   �����P                ��                      ��                  �  �                  ���                       �  �      g   �  ��         ��\�                           `�          0�  �      ��                  �      H�              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �P                      3   �����P  ̰     
   ��                      3   �����P         
   �                      3   �����P    ��                              ��        �                  ����                                        ��              P      ��                      g                               ��  g   �  б          ��	d�                           ��          h�  P�      ��                  �  �  ��              ���                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  Ĳ     Բ  Q                      3   �����P            ��                      3   ����Q    ��                              ��        �                  ����                                        �              Q      �                      g                               ȵ  g   �  س          ��	l�                           ��          p�  X�      ��                  �  �  ��              D �                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ̴     ܴ  PQ                      3   ����4Q            ��                      3   ����XQ    ��                              ��        �                  ����                                        �              R      �                      g                               D�    �  �  `�      tQ      4   ����tQ                p�                      ��                  �  �                  H�                       �  ��  ܶ  /   �  ��     ��                          3   �����Q            ̶                      3   �����Q      /   �  �     �                          3   �����Q  H�     
   8�                      3   �����Q  x�        h�                      3   �����Q  ��        ��                      3   �����Q            ȷ                      3   ����R  displayObjects  ��  ط                      S      �                               �                     ܼ    ^  `�  ܸ      4R      4   ����4R                �                      ��                  _  �                  `�                       _  p�  ��  /   `  �     (�                          3   ����DR            H�                      3   ����dR  �R     
                �R                     LT  @        
 T              � ߱        �  V   k  X�  ���                        �  /   �  �     $�                          3   ����`T  T�     
   D�                      3   �����T  ��        t�                      3   �����T  ��        ��                      3   �����T            Ժ                      3   �����T  �  /   �  �      �                          3   �����T  P�     
   @�                      3   �����T  ��        p�                      3   ���� U  ��        ��                      3   ����U            л                      3   ����4U      /   �  �     �                          3   ����PU  L�     
   <�                      3   ����pU  |�        l�                      3   ����xU  ��        ��                      3   �����U            ̼                      3   �����U  ��  g   �  ��         �48�                           ��          ��  t�      ��                  �      ��              �                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  �         �U                      3   �����U    ��                            ����                                        �              T      ��                      g                               L�  g   �  ��          �0�      }                      t�          D�  ,�      ��                  �      \�              |�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��         V                      3   �����U    ��                            ����                                        ��              U      ��                      g                               ��  $  �  x�  ���                       V                         � ߱        h�  $  �  ��  ���                       DV                         � ߱          x�      ��  ��                      ��        0         �  �                  D�      �V     ��     �  ��      $  �  ��  ���                       dV                         � ߱        (�  $  �  ��  ���                       �V                         � ߱            4   �����V  �V                     W                         � ߱            $  �  8�  ���                       �  $  �  ��  ���                       �W                         � ߱        ��  $  �  0�  ���                       X                         � ߱          ��      0�  ��                      ��        0         �  �                  ��      �X     p�     �  \�      $  �  �  ���                       0X                         � ߱        ��  $  �  \�  ���                       `X                         � ߱            4   �����X  �X                     �X                         � ߱            $  �  ��  ���                       �Y     
                 Z                     p[  @        
 0[              � ߱         �  V   �  �  ���                        |[     
                �[                     H]  @        
 ]              � ߱        ,�  V   �  ��  ���                        ��      H�  ��      T]      4   ����T]  t]     
                �]                     @_  @        
  _              � ߱            V     X�  ���                                        ��          l�  T�      ��                  X  b  ��              �'�                    O   ����    e�          O   ����    R�          O   ����    ��          O   `  ��  ��  T_    ��                            ����                            �  �      ��              V      ��                      
�     �                     |_  @         h_          �_  @         �_              � ߱        ��  $   y  P�  ���                       ��  g   �  ��          ��	<�                            ��          t�  \�      ��                  �  �  ��              �#�                    O   ����    e�          O   ����    R�          O   ����    ��            �  �_          ��                              ��        �                    ��        �                  ����                                        ��              W      ��                      g                               l�  g   �  ��          ��	�                            x�          H�  0�      ��                  �  �  `�              D$�                    O   ����    e�          O   ����    R�          O   ����    ��            �  �_          ��                              ��        �                    ��        �                  ����                                        ��              X      ��                      g                               D�  g   �  ��         �@��                            L�          �  �      ��                  �  �  4�              P/�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  x�         �_                      3   �����_    ��                              ��        �                  ����                                        ��              Y      ��                      g                               �  g   �  \�         �B��                            $�          ��  ��      ��                  �  �  �              00�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  P�         `                      3   ����`    ��                              ��        �                  ����                                        p�              Z      `�                      g                               ��  g   �  4�         � ��                            ��          ��  ��      ��                  �  �  ��              H+�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  (�         4`                      3   ���� `    ��                              ��        �                  ����                                        H�              [      8�                      g                               ��  g   �  �         �Op�                            ��          ��  ��      ��                  �    ��              <,�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �   �         P`                      3   ����<`    ��                              ��        �                  ����                                         �              \      �                      g                               ��  g   
  ��         �NH�                            ��          |�  d�      ��                      ��              p3�                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��         l`                      3   ����X`    ��                              ��        �                  ����                                        ��              ]      ��                      g                               ��  g     ��         �~d�                            ��          T�  <�      ��                    .  l�              h4�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  )  ��         �`                      3   ����t`        *  ��  ��      �`      4   �����`      O  +  ������  �`    ��                              ��        �                  ����                                        ��              ^      �                      g                               ��  g   5  ��         ���                            ��          p�  X�      ��                  6  D  ��              �7�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  @  ��         �`                      3   �����`        A  ��  �      �`      4   �����`      O  B  ������  �`    ��                              ��        �                  ����                                        ��              _       �                      g                               �  g   K  ��         ϊ��                             �          ��  t�      ��                 L  0  ��              �8�                    O   ����    e�          O   ����    R�          O   ����    ��      a     
                �a                     �b  @        
 �b              � ߱        ��  V   f  ��  ���                        �b     
                hc                     xd                         � ߱        ��  $  �  L�  ���                             �  ��  t�  ��  �d      4   �����d                ��                      ��                  �  �                  �H�                       �  �      /  �  ��         4e                      3   ���� e        �  ��  X�      <e      4   ����<e                ��                      ��                  �  (                  �K�                       �  ��  He     
                �e                     �f                         � ߱        \�  $  �  h�  ���                       g     
                �g                     �h     
                    � ߱        ��  $  �  ��  ���                       ��  $     ��  ���                       �h                         � ߱            p     <i  ��      '  ��  x�     Hi                ��                      ��                                      �L�                         �      /    ��         hi                      3   ����Ti      @�     pi                P�                      ��                    &                  �O�                         ��      /    |�         �i                      3   ����|i               (�           �  �   T ��                          
                                             $   4   D          $   4   D    �          ��                              ��        �                    ��        �                  ����                            ��          �      ��     `     4�                      g   0�                              g   7  (�         �4��                            ��          ��  ��      ��                  8  D  ��              �P�                    O   ����    e�          O   ����    R�          O   ����    ��          /  A  �         �i                      3   �����i    ��                              ��        �                  ����                                        <�              a      ,�                      g                               disable_UI  ��  ��                      b                                      
                    �� �   ���  �         �  ��              8   ����        8   ����        ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  <�  H�      returnFocus ,INPUT hTarget HANDLE   ,�  p�  ��      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    `�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  0�      removeAllLinks  ,   �  D�  T�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE 4�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  8�  D�      hideObject  ,   (�  X�  d�      exitObject  ,   H�  x�  ��      editInstanceProperties  ,   h�  ��  ��      displayLinks    ,   ��  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  (�  8�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  P�  `�      processAction   ,INPUT pcAction CHARACTER   @�  ��  ��      enableObject    ,   |�  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  ��  �      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    ��  @�  L�      updateMode  ,INPUT pcMode CHARACTER 0�  t�  ��      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  d�  ��  ��      queryPosition   ,INPUT pcState CHARACTER    ��  ��  �      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   ��  T�  d�      dataAvailable   ,INPUT pcRelative CHARACTER D�  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  ��  �  �      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  ��  L�  \�      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER <�  ��  ��      viewObject  ,   ��  ��  ��      updateTitle ,   ��  ��  ��      updateState ,INPUT pcState CHARACTER    ��  $�  4�      updateRecord    ,   �  H�  P�      toolbar ,INPUT pcValue CHARACTER    8�  |�  ��      setDown ,INPUT piNumDown INTEGER    l�  ��  ��      searchTrigger   ,   ��  ��  ��      rowDisplay  ,   ��  ��  �      resizeObject    ,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL ��  D�  T�      resizeBrowse    ,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL 4�  ��  ��      resetRecord ,   ��  ��  ��      refreshBrowse   ,   ��  ��  ��      offHome ,   ��  ��  ��      offEnd  ,   ��  �  $�      initializeObject    ,    �  8�  H�      filterActive    ,INPUT plActive LOGICAL (�  p�  ��      fetchDataSet    ,INPUT pcState CHARACTER    `�  ��  ��      enableFields    ,   ��  ��  ��      displayFields   ,INPUT pcColValues CHARACTER    ��  �   �      disableFields   ,INPUT pcFields CHARACTER    �  L�  \�      destroyObject   ,   <�  p�  ��      deleteRecord    ,   `�  ��  ��      deleteComplete  ,   ��  ��  ��      defaultAction   ,   ��  ��  ��      copyRecord  ,   ��  ��  �      cancelRecord    ,   ��   �  ,�      calcWidth   ,   �  @�  P�      assignMaxGuess  ,INPUT piMaxGuess INTEGER   0�  |�  ��      applyEntry  ,INPUT pcField CHARACTER    l�  ��  ��      applyCellEntry  ,INPUT pcCellName CHARACTER ��  ��  ��      addRecord   ,       "       "        �     }        �� �  D   %               � 
"    
 �%              � ��  �         �      \     H     $              
�    �    �     
�             �G�    �G     
�             �G                      
�            �      
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �            7%               
"   
 ��           8    1�   
 �� "   �%               o%   o           � '    �
"   
 ��           �    1� (   �� "   �%               o%   o           � 6   �
"   
 ��                1� =  
 �� "   �%               o%   o           � H   �
"   
 ��           �    1� Y   �� "   �%               o%   o           � '    �
"   
 ��               1� g   �� "   �%               o%   o           � v   �
"   
 ��           |    1� �   �� �   �%               o%   o           %               
"   
 ��          �    1� �   �� �     
"   
 ��           4    1� �   �� "   �%               o%   o           � �  � �
"   
 ��           �    1� |   �� "   �%               o%   o           � �  ( �
"   
 ��               1� �   �� �   �%               o%   o           %               
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��               1� �   �� �   �%               o%   o           %              
"   
 ��          �    1� �   �� �     
"   
 ��           �    1� �  
 �� �   �%               o%   o           %               
"   
 ��           H    1� �   �� "   �%               o%   o           � '    �
"   
 ��          �    1�    �� �     
"   
 ��           �    1�    �� "   �%               o%   o           � +  t �
"   
 ��          l	    1� �  
 �� �     
"   
 ��           �	    1� �   �� "   �%               o%   o           � �  � �
"   
 ��           
    1� I   �� "   �%               o%   o           � '    �
"   
 ��           �
    1� `  
 �� k   �%               o%   o           %               
"   
 ��               1� o   �� �   �%               o%   o           %               
"   
 ��           �    1� w   �� "   �%               o%   o           � '    �
"   
 ��           �    1� �   �� "   �%               o%   o           o%   o           
"   
 ��           x    1� �  
 �� "   �%               o%   o           � '    �
"   
 ��           �    1� �   �� �  	 �%               o%   o           � �  / �
"   
 ��          `    1� �   �� �  	   
"   
 ��           �    1�     �� �  	 �o%   o           o%   o           � '    �
"   
 ��              1�    �� �  	   
"   
 ��           L    1� "   �� �  	 �o%   o           o%   o           � '    �
"   
 ��          �    1� 2   �� �     
"   
 ��          �    1� @   �� �  	   
"   
 ��          8    1� M   �� �  	   
"   
 ��          t    1� Z   �� �  	   
"   
 ��           �    1� h   �� �   �o%   o           o%   o           %              
"   
 ��          ,    1� y   �� �  	   
"   
 ��          h    1� �  
 �� �     
"   
 ��          �    1� �   �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��          X    1� �   �� �  	   
"   
 ��          �    1� �  	 �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��           H    1�    �� "   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
 �(�  L ( l       �            ��     � P   �            �@    
� @  , 
�       (    �� )     p�               �L
�    %              � 8      4    � $         � 0          
�    � J     
"   
 �� @  , 
�       D    �� =  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 ��           �    1� M   �� �  	 �%               o%   o           � '    �
"   
 ��           d    1� Z   �� �  	 �%               o%   o           � '    �
"   
 ��           �    1� h   �� �   �%               o%   o           %               
"   
 ��           T    1� v   �� �  	 �%               o%   o           � '    �
"   
 ��           �    1� �   �� �  	 �%               o%   o           � '    �
"   
 ��           <    1� �   �� �   �%               o%   o           %               
"   
 ��           �    1� �   �� �  	 �%               o%   o           � '    �
"   
 ��           ,    1� �   �� �  	 �%               o%   o           � '    �
"   
 ��           �    1� �   �� �  	 �%               o%   o           � '    �
"   
 ��               1� �   �� �  	 �%               o%   o           o%   o           
"   
 ��           �    1� �   �� �  	 �%               o%   o           � '    �
"   
 ��               1� �   �� �  	 �%               o%   o           � '    �
"   
 ��           x    1� �  	 �� �   �%               o%   o           %               
"   
 ��           �    1�    �� �   �%               o%   o           %               
"   
 ��           p    1�    �� �   �%               o%   o           o%   o           
"   
 ��           �    1�    �� �   �%               o%   o           o%   o           
"   
 ��           h    1� ,   �� �   �%               o%   o           %               
"   
 ��           �    1� :   �� �   �%               o%   o           %               
"   
 ��           `    1� K   �� �   �%               o%   o           %               
"   
 ��           �    1� `   �� l   �%               o%   o           %       
       
"   
 ��           X    1� t   �� l   �%               o%   o           o%   o           
"   
 ��           �    1� �   �� l   �%               o%   o           %              
"   
 ��           P    1� �   �� l   �%               o%   o           o%   o           
"   
 ��           �    1� �   �� l   �%               o%   o           %              
"   
 ��           H     1� �   �� l   �%               o%   o           o%   o           
"   
 ��           �     1� �   �� l   �%               o%   o           %              
"   
 ��           @!    1� �   �� l   �%               o%   o           o%   o           
"   
 ��           �!    1� �   �� �  	 �%               o%   o           � '    �P �L 
�H T   %              �     }        �GG %              
"   
 ��           �"    1� �   �� "   �%               o%   o           � '    �
"   
 ��           �"    1� �   �� �   �%               o%   o           %               
"   
 ��           t#    1� �   �� "   �%               o%   o           � '    �
"   
 ��     ,      �#    1� �   �� "   �%               o%   o           �   �      �    ��   	 �
"   
 ��           |$    1�    �� �   �%               o%   o           o%   o           
"   
 ��           �$    1� $   �� "   �%               o%   o           � '    �
"   
 ��           l%    1� 2   �� "   �%               o%   o           � '    �
"   
 ��           �%    1� A   �� �  	 �%               o%   o           o%   o           
"   
 ��           \&    1� Y   �� "   �%               o%   o           o%   o           
"   
 ��           �&    1� h   �� "   �%               o%   o           � '    �
"   
 ��           L'    1� u   �� �   �%               o%   o           %               
"   
 ��          �'    1� �   �� �     
"   
 ��           (    1� �   �� "   �%               o%   o           � �  ~ �
"   
 ��           x(    1� ,   �� "   �%               o%   o           � '    �
"   
 ��           �(    1� >   �� "   �%               o%   o           � V   �
"   
 ��           `)    1� l   �� �  	 �%               o%   o           � �   �
"   
 ��           �)    1� �   �� �  	 �%               o%   o           � �   �
"   
 ��           H*    1� �  	 �� "   �%               o%   o           � �   �
"   
 ��           �*    1� �  
 �� �  	 �%               o%   o           � �   �
"   
 ��           0+    1� �   �� �   �%               o%   o           o%   o           
"   
 ��           �+    1� �   �� "   �%               o%   o           � �   �
"   
 ��            ,    1� �   �� "   �%               o%   o           � '    �
"   
 ��           �,    1� �  
 �� �   �%               o%   o           o%   o           
"   
 ��          -    1� �   �� �     
"   
 ��           L-    1�    �� "   �%               o%   o           �   ] �
"   
 ��           �-    1� z   �� "   �%               o%   o           � '    �
"   
 ��           4.    1� �   �� "   �%               o%   o           � �   �
"   
 ��           �.    1� �   �� �   �%               o%   o           %               
"   
 ��           $/    1� �   �� "   �%               o%   o           � '    �
"   
 ��           �/    1� �   �� "   �%               o%   o           o%   o           
"   
 ��          0    1� �   �� �  	   P �L 
�H T   %              �     }        �GG %              
"   
 ��           �0    1� �   �� l   �%               o%   o           o%   o           
"   
 ��           1    1� �   �� �     
"   
 ��           \1    1� �   �� �   �%               o%   o           %               
"   
 ��           �1    1�   	 �� �   �%               o%   o           %               
"   
 ��           T2    1�    �� �   �%               o%   o           %       P       
"   
 ��           �2    1�    �� "   �%               o%   o           � '    �
"   
 ��           D3    1� *   �� l   �%               o%   o           %               
"   
 ��           �3    1� 2   �� "   �%               o%   o           � '    �
"   
 ��          44    1� >   �� �     
"   
 ��          p4    1� K   �� "     
"   
 ��          �4    1� W   �� i     
"   
 ��          �4    1� m   �� i     
"   
 ��          $5    1�    �� i     
"   
 ��          `5    1� �   �� �     
"   
 ��          �5    1� �   �� "     
"   
 ��          �5    1� �   �� i     
"   
 ��           6    1� �   �� "   �%               o%   o           � '    �
"   
 ��           �6    1� �   �� �   �%               o%   o           %              
"   
 ��           7    1� �   �� �   �%               o%   o           %              
"   
 ��           �7    1� �   �� �   �%               o%   o           %               
"   
 ��           �7    1� �   �� �   �%               o%   o           %               
"   
 ��          x8    1�    �� �     
"   
 ��          �8    1�    �� �     
"   
 ��          �8    1� (   �� "     
"   
 ��          ,9    1� 8   �� "     
"   
 ��           h9    1� J  
 �� �   �%               o%   o           %              
"   
 ��          �9    1� U   �� "     
"   
 ��           :    1� j   �� "     
"   
 ��          \:    1�    �� "     
"   
 ��          �:    1� �   �� "     
"   
 ��          �:    1� �   �� "     
"   
 ��          ;    1� �   �� "     
"   
 ��          L;    1� �   �� "     
"   
 ��          �;    1� �   �� �  	   
"   
 ��          �;    1�    �� �  	   
"   
 ��           <    1�    �� �  	   
"   
 ��          <<    1� (   �� �  	   
"   
 ��          x<    1� ?   �� �  	   
"   
 ��          �<    1� Q   �� �  	   
"   
 ��          �<    1� g   �� �  	   
"   
 ��          ,=    1� ~   �� �  	   
"   
 ��          h=    1� �   �� �  	   
"   
 ��          �=    1� �   �� �  	   
"   
 ��          �=    1� �   �� �  	   
"   
 ��          >    1� �   �� �  	   
"   
 ��           X>    1� �   �� �   �%               o%   o           %              
�             �G "    �%     start-super-proc �%     adm2/smart.p ��P �L 
�H T   %              �     }        �GG %              
"   
   �       �?    6�       
"   
   
�        �?    8
"   
   �        �?    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   ScrollRemote,NumDown,CalcWidth,MaxWidth,FetchOnReposToEnd,UseSortIndicator,SearchField,DataSourceNames,UpdateTargetNames,LogicalObjectName,HideOnInit,DisableOnInit,ObjectLayout ��
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        hA    ��     � P   �        tA    �@    
� @  , 
�       �A    �� )   �p�               �L
�    %              � 8      �A    � $         � 0          
�    � J   �
"   
 �p� @  , 
�       �B    �� �   �p�               �L"    , �   �     �� "   ��     }        �A      |    "      �     �%              (<   \ (    |    �     }        �A� $   �A"  	  �    "    �"  	  �  < "    �"  	  �(    |    �     }        �A� $   �A"  	  �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        pD    ��     � P   �        |D    �@    
� @  , 
�       �D    �� )   �p�               �L
�    %              � 8      �D    � $         � 0          
�    � J   �
"   
 �p� @  , 
�       �E    ��   
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        HF    ��     � P   �        TF    �@    
� @  , 
�       `F    �� )   �p�               �L
�    %              � 8      lF    � $         � 0   �     
�    � J   �
"   
 �p� @  , 
�       |G    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 �
"   
   
"   
   (�  L ( l       �        (H    ��     � P   �        4H    �@    
� @  , 
�       @H    �� )     p�               �L
�    %              � 8      LH    � $         � 0          
�    � J     
"   
 �p� @  , 
�       \I    �� =  
 �p�               �L%     SmartDataBrowser  �
"   
   p� @  , 
�       �I    �� Y     p�               �L%               
"   
  p� @  , 
�       (J    �� "    p�               �L%               
"   
  p� @  , 
�       �J    ��      p�               �L(        � '      � '      � '      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 �    �        hK    ��     �
"   
   � 8      �K    � $         � 0          
�    � J   �
"   
   �        L    �
"   
   �       ,L    /
"   
   
"   
   �       XL    6�       
"   
   
�        �L    8
"   
   �        �L    �
"   
   �       �L    �
"   
   p�    � M   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �M    �A"    �A
"   
   
�        �M    �@ � 
"   
 �"      �       }        �
"   
 �%              %                "    �%     start-super-proc �%     adm2/visual.p ��   �      �      � �     
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        $O    ��     � P   �        0O    �@    
� @  , 
�       <O    �� )   �p�               �L
�    %              � 8      HO    � $         � 0          
�    � J   �
"   
 �p� @  , 
�       XP    �� �   �p�               �L"  
  , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ��%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/datavis.p %     modifyListProperty 
�    %      ADD     %     SupportedLinks %     Toolbar-Target  "    �%     start-super-proc �%     adm2/browser.p 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �R    ��     � P   �        �R    �@    
� @  , 
�       �R    �� )   �p�               �L
�    %              � 8      �R    � $         � 0   �     
�    � J   �
"   
 �p� @  , 
�        T    �� �   �p�               �L
�             �G%     modifyListProperty 
�    %      ADD     %     DataSourceEvents ��%     FilterActive nts%     modifyListProperty 
�    %      ADD     %     DataSourceEvents ��%     RefreshBrowse ts%     modifyListProperty 
�    %      ADD     %     DataSourceEvents ��% 	    CancelNew Ev%     valueChanged    
�    %     valueChanged    
�    �,            $     � �   ߱        � �  
 ��    "      �          %              %                   "      %                  "      "      "     T   "    �"    ��    � T h     @   "      (        "    �� '      �    �� '    �(  4  8    "    �� �  
 �T   %              "    �� $   �"      �,            $     � '    ߱        � �  
 ��    "      �          %              %                   "      %                  "      "      "     T   "    �"    ��    � T h     @   "      (        "    �� '      �    �� '    �(  4  8    "    �� �  
 �T   %              "    �� $   �"      
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �Y    ��     � P   �        �Y    �@    
� @  , 
�       Z    �� )   �p�               �L
�    %              � 8      Z    � $         � 0          
�    � J   �
"   
 �p� @  , 
�       $[    �� �   �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �[    ��     � P   �        �[    �@    
� @  , 
�       �[    �� )   �p�               �L
�    %              � 8      �[    � $         � 0          
�    � J   �
"   
 �p� @  , 
�       �\    �� $   �p�               �L"    ,     "    �� '    �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �]    ��     � P   �        �]    �@    
� @  , 
�       �]    �� )   �p�               �L
�    %              � 8      �]    � $         � 0   �     
�    � J   �
"   
 �p� @  , 
�       �^    �� u   �p�               �L%              
�     
         �G�             I%               �             �%              %      END     %      HOME    %     defaultAction   
�    %      onEnd   
�    %      onHome  
�    %      offEnd  
�    %      offHome 
�    %     rowEntry ��
�        �  � �  	 �%               %     rowLeave ��
�        �  � �  	 �%               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        Xa    ��     � P   �        da    �@    
� @  , 
�       pa    �� )   �p�               �L
�    %              � 8      |a    � $         � 0   �     
�    � J   �
"   
 �p� @  , 
�       �b    �� �   �p�               �L%              
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        8c    ��     � P   �        Dc    �@    
� @  , 
�       Pc    �� )   �p�               �L
�    %              � 8      \c    � $         � 0   �     
�    � J     
"   
 �� @  , 
�       ld    ��    �p�               �L0 0       �             �%                   �             ��             <%      offEnd  
�    "      
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        �e    ��     � P   �        �e    �@    
� @  , 
�       �e    �� )   �p�               �L
�    %              � 8      �e    � $         � 0   �     
�    � J     
"   
 �� @  , 
�       �f    �� �   �p�               �L
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        `g    ��     � P   �        lg    �@    
� @  , 
�       xg    �� )   �p�               �L
�    %              � 8      �g    � $         � 0   �     
�    � J     
"   
 �
� @  , 
�       �h    �� �   �p�               �L�P            $     "    ߱                $     
"   
 �        �   
 �"      �      %      offHome 
�    �      %      offEnd  
�    %     onValueChanged  
�    �     }        �
�                    �           �   l       ��                 T  x  �               0Ǻ                    O   ����    e�          O   ����    R�          O   ����    ��        $  c  �   ���                       K     
                    � ߱              d  (  �      tK      4   ����tK                �                      ��                  e  w                  ��                       e  8  �  �  f  �K            h  �  `      L      4   ����L                p                      ��                  i  v                  ���                       i  �  �  o   j      ,                                 �  �   k  8L      �  �   l  dL      $  $  m  �  ���                       �L     
                    � ߱        8  �   n  �L      L  �   o  �L      `  �   r  �L          $   u  �  ���                        M  @         M              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      n                      �          �  $  �    ���                       tM     
                    � ߱                  �  �                      ��                   �  �                  <��                     �  4      4   �����M      $  �  �  ���                       �M     
                    � ߱        �    �  4  D      �M      4   �����M      /  �  p                               3   ����N  �  �   �  N          O   �  ��  ��  LN                               , �                          
                               �      ��                            ����                                                        �   l       ��                    9  �               X�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  \  h  �               8T�                    O   ����    e�          O   ����    R�          O   ����    ��      �      f  �� �                       g  �         �i      4   �����i      �   g  �i    ��                              ��        �                  ����                                ��          �  �	   Æ�                              
 �                                                                 �  �     �         )                                    
 �                                                                �  �     �  (       1                                      �                                                                                                                                       �   d d     t   ���  �  � �                                               �                                                                         d     D                                                                 H  d d ��                                  �          �            D                                                                    TXS ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST RowObject CodCia CodAlm Descripcion RowNum RowIdent RowMod RowIdentIdx RowUserProp br_table x(3) X(40) F-Main O:\OpenEdge\on_in_co\APLIC\btablewi.w should only be RUN PERSISTENT. glReposition cLastEvent COLVALUES GETACTIONEVENT GETAPPLYACTIONONEXIT GETAPPLYEXITONACTION GETBROWSEHANDLE GETCALCWIDTH GETDATASIGNATURE GETMAXWIDTH GETNUMDOWN GETQUERYROWOBJECT GETSCROLLREMOTE GETSEARCHFIELD GETTARGETPROCEDURE GETVISIBLEROWIDS GETVISIBLEROWRESET ROWVISIBLE SETACTIONEVENT SETAPPLYACTIONONEXIT SETAPPLYEXITONACTION SETCALCWIDTH SETDATAMODIFIED SETMAXWIDTH SETNUMDOWN SETQUERYROWOBJECT SETSCROLLREMOTE SETSEARCHFIELD SETVISIBLEROWIDS SETVISIBLEROWRESET STRIPCALCS GETOBJECTTYPE GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataBrowser ContainerType PropertyDialog adm2/support/browsed.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties ScrollRemote,NumDown,CalcWidth,MaxWidth,FetchOnReposToEnd,UseSortIndicator,SearchField,DataSourceNames,UpdateTargetNames,LogicalObjectName,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks TableIO-Target,Data-Target,Update-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CreateHandles DataModified DisplayedFields DisplayedTables   rowObject Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTarget UpdateTargetNames WindowTitleField SeparatorFGColor BrowseHandle BrowseInitted CalcWidth MaxWidth ModifiedFields NumDown SearchField SearchHandle ActionEvent ApplyActionOnExit LOG ApplyExitOnAction ScrollRemote QueryRowObject VisibleRowids VisibleRowReset FolderWindowToLaunch FetchOnReposToEnd PopupActive ColumnsMovable ColumnsSortable MovableHandle SortableHandle SavedColumnData DefaultColumnData Separators BrowseColumnBGColors BrowseColumnFGColors BrowseColumnLabelBGColors BrowseColumnLabelFGColors BrowseColumnLabelFonts BrowseColumnLabels BrowseColumnWidths BrowseColumnFormats BrowseColumnFonts BrowseColumnTypes BrowseColumnDelimiters BrowseColumnItems BrowseColumnItemPairs BrowseColumnInnerLines BrowseColumnSorts BrowseColumnMaxChars BrowseColumnAutoCompletions BrowseColumnUniqueMatches Tooltip UseSortIndicator ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry cBaseQuery hQuery cColumns iTable hColumn lResult cStripDisp cStripEnable adm2/browser.p FilterActive RefreshBrowse CancelNew CodAlm Descripcion stripCalcs RowObject. GETROWOBJECT END HOME adm-error cRowVis hRowObj lScrollRemote cRowids rowVisible FIRST LAST DISABLE_UI Almac�n Descripci�n �  D#  �  H+      . �    ��      0         pcCellName      ��      T         pcField     ��      t         piMaxGuess      ��      �         pcFields        ��      �         pcColValues     ��      �         pcState     ��               plActive    0  ��      $        pd_height       ��      H        pd_height   x  ��      l        pd_height       ��      �        pd_height       ��      �        piNumDown       ��      �        pcValue     ��      �        pcState $  ��              pcChanges       ��      <        pcChanges       ��      `        plCancel        ��      �        plAnswer        ��      �        plCancel        ��      �        pcRelative  �  ��      �        pcAction        ��              pcAction        ��      8        pcState     ��      X        pcReturn        ��      |        pcMode      ��      �        pcState     ��      �        pcNotValidFields        ��      �        pcAction      ��             
 phSource    <  ��      0        phSource        ��      T       
 phSource    �  ��      x        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��      �       
 phObject        ��               phObject        ��      D        pcField     ��      d        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller       ��              pcMod   @  ��      8        pcMod       ��      X       
 pcMod   �  ��      x       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      $       
 hTarget P  ��      D        pcMessage       ��      h        pcMessage       ��      �        plEnabled             �     cType       �     M   �          �                  getObjectType   L	  d	  f	  $          
   hReposBuffer    D        8  
   hPropTable  `        X  
   hBuffer           t  
   hTable  �  �     N              �                  adm-clone-props c  d  e  f  h  i  j  k  l  m  n  o  r  u  v  w  x            	  
   hProc             4	        pcProcName  |  �	  	   O    	  	      p	                  start-super-proc    �  �  �  �  �  �  �  �  �  @	  �	     P                                   �  �	  
     Q                                   �  �  �	  D
     R                                   �  �  
  �
     S               |
                  displayObjects  9  L
  �
     T                                   �  �
  �
     U                                   �  �
  8     V               (                  getRowObject    `  b  �
  p     W                                   �  �  @  �     X                                   �  �  x  �     Y                                   �  �  �       Z                                   �  �  �  P     [                                   �  �     �     \                                   �    X  �     ]                                       �  �     ^                                   )  *  +  .  �  8     _                                   @  A  B  D  d        \     cRowVis �        x  
   hRowObj �        �     lScrollRemote             �     cRowids   �     `   H                              f  �  �  �  �  �  �  �  �                 &  '  (  0  �  p     a                                   A  D  @  �     b               �                  disable_UI  f  g  h  x  h  �      �      L                                  RowObject   l         t         |         �         �         �         �         �         CodCia  CodAlm  Descripcion RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp �       �     glReposition            �     cLastEvent  (          
   gshAstraAppserver   P        <  
   gshSessionManager   t        d  
   gshRIManager    �        �  
   gshSecurityManager  �        �  
   gshProfileManager   �        �  
   gshRepositoryManager              
   gshTranslationManager   @  	 	     0  
   gshWebManager   d  
 
     T     gscSessionId    �        x     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager   �        �  
   gshAgnManager                gsdTempUniqueID 8        ,     gsdUserObj  `        L     gsdRenderTypeObj    �        t     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps  �       �  
   ghADMPropsBuf          �     glADMLoadFromRepos  ,       $     glADMOk L       @  
   ghContainer l       `     cObjectName �    	   �     iStart  �    
   �     cFields �       �     cViewCols   �       �     cEnabled            �     iCol                 iEntries    <       4     cEntry  \       P     cBaseQuery  x       p  
   hQuery  �       �     cColumns    �       �     iTable  �       �  
   hBuffer �       �  
   hColumn              lResult (            cStripDisp           <     cStripEnable            X  \  RowObject      @  A  C  D          4  @  A  B  D  F  G  H  L  M  P  Q  R  S  U  W  Y  [  \  ]  `  b  c  e  f  g  h  i  o  q  w  y  {  |  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  
  %
  &
  (
  )
  *
  +
  1
  2
  3
  4
  5
  6
  7
  8
  9
  ;
  <
  =
  >
  ?
  @
  A
  B
  C
  D
  E
  F
  H
  I
  K
  L
  M
  N
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
  m  }  �  �  �  �  �  �  �  �  �  �  �    )  E  G  \  �  �  �      �  �  �  �  �  �  �  �  �  �  �  ^  _  `  k  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �      y  �  �  �  �  �  �  
    5  K  7      N - C:\Progress\OpenEdge\src\adm2\brschnge.i P  � , C:\Progress\OpenEdge\src\adm2\brsscrol.i �  l� + C:\Progress\OpenEdge\src\adm2\brsleave.i �  0 * C:\Progress\OpenEdge\src\adm2\brsentry.i �  �� ) C:\Progress\OpenEdge\src\adm2\brsoffhm.i    �J ( C:\Progress\OpenEdge\src\adm2\brsoffnd.i T  ] ' C:\Progress\OpenEdge\src\adm2\brshome.i  �  Џ & C:\Progress\OpenEdge\src\adm2\brsend.i   �  �� % C:\Progress\OpenEdge\src\adm2\brsdefault.i   �  ��  C:\Progress\OpenEdge\src\adm2\browser.i  (  'z $ %C:\Progress\OpenEdge\src\adm2\custom\browsercustom.i \  }  C:\Progress\OpenEdge\src\adm2\datavis.i  �  � # %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i �  ��  C:\Progress\OpenEdge\src\adm2\visual.i     # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  D  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds ! C:\Progress\OpenEdge\gui\fn  �  tw   %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   �  Q.  C:\Progress\OpenEdge\gui\set    ��  C:\Progress\OpenEdge\src\adm2\brsprop.i  H  ��  %C:\Progress\OpenEdge\src\adm2\custom\brspropcustom.i |  !&  %C:\Progress\OpenEdge\src\adm2\custom\brsprtocustom.i �  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i �  B�  %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    0  ��  %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    t  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i ,  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i l  �j  C:\Progress\OpenEdge\gui\get �  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i       ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i P   Su  C:\Progress\OpenEdge\src\adm2\globals.i  �   M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �   )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �   �X  C:\Progress\OpenEdge\src\adm2\visprto.i  <!  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  p!  �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �!  0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i �!  _�  C:\Progress\OpenEdge\src\adm2\brsprto.i  ,"  t�  %C:\Progress\OpenEdge\src\adm2\custom\browserdefscustom.i `"  ��  C:\Progress\OpenEdge\src\adm2\robjflds.i �"  �h  O:\OpenEdge\on_in_co\aplic\q-almcen.i    �"  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   #  I�   O:\OpenEdge\on_in_co\APLIC\btablewi.w        6  D      x#     :  -   �#  ,  0      �#  *   &  ,   �#       (   �#  &     ,   �#       )   �#  !      ,   �#     �     �#      �  ,   $     �     $     �  ,   ($     �  (   8$     �  ,   H$     �     X$     �  ,   h$     ^     x$     N  ,   �$  "  D      �$     8  +   �$    .      �$     !  *   �$          �$       )   �$          �$     �  (   %  �   �      %     �  '   (%  �   �      8%     �  &   H%  �   �      X%     �  %   h%  �   j      x%  �   �     �%     :  $   �%  �   5     �%          �%  �        �%     �     �%  �   �     �%     �     �%  �   �     &     c     &  a   A     (&  o        8&     �  #   H&  W   �     X&  n   �     h&     ,  "   x&  i   '     �&          �&  N   �     �&  �   t     �&     r  !   �&  �   B     �&     �      �&  �   �     �&     �     '  �   �     '     �     ('  �   �     8'     w     H'  �   v     X'     T     h'  �   C     x'     !     �'  �        �'     �     �'  }   �     �'     �     �'     R     �'          �'     �     �'  (   v     (  �   m     (  O   _     ((     N     8(           H(  �   �
     X(  �   �
     h(  O   �
     x(     �
     �(     S
     �(  }   "
     �(  �   
     �(  O   
     �(     �	     �(     �	     �(  �   �	     �(  �  c	     )     D	     )  �  	     ()  O   	     8)     �     H)     �     X)  �   �     h)     �     x)     �     �)  x   �     �)     �     �)     _     �)     [     �)     G     �)     .     �)  f        �)     �     *  "   a     *     M     (*     ,     8*  X        H*     Q  
   X*           h*       	   x*     �     �*  b   �     �*     �     �*     �     �*     �     �*     ~     �*     W     �*  ^         �*          +  ]          +     �      (+  '   �       8+     '      