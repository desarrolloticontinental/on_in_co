	��V�Bfx2    �              �                                 � 32780106utf-8 MAIN D:\newsie\on_in_co\aplic\alm\balmacen.w,, PROCEDURE disable_UI,, PROCEDURE displayObjects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE exitObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE validateFields,,INPUT-OUTPUT pcNotValidFields CHARACTER PROCEDURE updateMode,,INPUT pcMode CHARACTER PROCEDURE showDataMessagesProcedure,,OUTPUT pcReturn CHARACTER PROCEDURE queryPosition,,INPUT pcState CHARACTER PROCEDURE okToContinueProcedure,,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE dataAvailable,,INPUT pcRelative CHARACTER PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE confirmDelete,,INPUT-OUTPUT plAnswer LOGICAL PROCEDURE confirmContinue,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE collectChanges,,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER PROCEDURE viewObject,, PROCEDURE updateTitle,, PROCEDURE updateState,,INPUT pcState CHARACTER PROCEDURE updateRecord,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE setDown,,INPUT piNumDown INTEGER PROCEDURE searchTrigger,, PROCEDURE rowDisplay,, PROCEDURE resizeObject,,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL PROCEDURE resizeBrowse,,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL PROCEDURE resetRecord,, PROCEDURE refreshBrowse,, PROCEDURE offHome,, PROCEDURE offEnd,, PROCEDURE initializeObject,, PROCEDURE filterActive,,INPUT plActive LOGICAL PROCEDURE fetchDataSet,,INPUT pcState CHARACTER PROCEDURE enableFields,, PROCEDURE displayFields,,INPUT pcColValues CHARACTER PROCEDURE disableFields,,INPUT pcFields CHARACTER PROCEDURE destroyObject,, PROCEDURE deleteRecord,, PROCEDURE deleteComplete,, PROCEDURE defaultAction,, PROCEDURE copyRecord,, PROCEDURE cancelRecord,, PROCEDURE calcWidth,, PROCEDURE assignMaxGuess,,INPUT piMaxGuess INTEGER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE applyCellEntry,,INPUT pcCellName CHARACTER PROCEDURE addRecord,, FUNCTION getRowObject,widget-handle, FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION showDataMessages,CHARACTER, FUNCTION setWindowTitleField,LOGICAL,INPUT cWindowTitleField CHARACTER FUNCTION setUpdateTargetNames,LOGICAL,INPUT pcTargetNames CHARACTER FUNCTION setUpdateTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setTableIOSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setTableIOSource,LOGICAL,INPUT phObject HANDLE FUNCTION setSaveSource,LOGICAL,INPUT plSave LOGICAL FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setLogicalObjectName,LOGICAL,INPUT pcLogicalObjectName CHARACTER FUNCTION setGroupAssignTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setGroupAssignSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setGroupAssignSource,LOGICAL,INPUT phObject HANDLE FUNCTION setEnabledFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setDisplayedFields,LOGICAL,INPUT pcFieldList CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT pcContainerMode CHARACTER FUNCTION okToContinue,LOGICAL,INPUT pcAction CHARACTER FUNCTION getWindowTitleField,CHARACTER, FUNCTION getUpdateTargetNames,CHARACTER, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getTableIOSourceEvents,CHARACTER, FUNCTION getTableIOSource,HANDLE, FUNCTION getRowIdent,CHARACTER, FUNCTION getRecordState,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getNewRecord,CHARACTER, FUNCTION getGroupAssignTargetEvents,CHARACTER, FUNCTION getGroupAssignTarget,CHARACTER, FUNCTION getGroupAssignSourceEvents,CHARACTER, FUNCTION getGroupAssignSource,HANDLE, FUNCTION getFieldsEnabled,LOGICAL, FUNCTION getFieldHandles,CHARACTER, FUNCTION getEnabledHandles,CHARACTER, FUNCTION getEnabledFields,CHARACTER, FUNCTION getDisplayedTables,CHARACTER, FUNCTION getDisplayedFields,CHARACTER, FUNCTION getDataModified,LOGICAL, FUNCTION getCreateHandles,CHARACTER, FUNCTION getObjectType,character, FUNCTION stripCalcs,CHARACTER,INPUT cClause CHARACTER FUNCTION setVisibleRowReset,LOGICAL,INPUT plReset LOGICAL FUNCTION setVisibleRowids,LOGICAL,INPUT pcRowids CHARACTER FUNCTION setSearchField,LOGICAL,INPUT pcField CHARACTER FUNCTION setScrollRemote,LOGICAL,INPUT plScrollRemote LOGICAL FUNCTION setQueryRowObject,LOGICAL,INPUT phQueryRowObject HANDLE FUNCTION setNumDown,LOGICAL,INPUT piNumDown INTEGER FUNCTION setMaxWidth,LOGICAL,INPUT pdMaxWidth DECIMAL FUNCTION setDataModified,LOGICAL,INPUT lModified LOGICAL FUNCTION setCalcWidth,LOGICAL,INPUT plCalcWidth LOGICAL FUNCTION setApplyExitOnAction,LOGICAL,INPUT plApply LOGICAL FUNCTION setApplyActionOnExit,LOGICAL,INPUT plApply LOGICAL FUNCTION setActionEvent,LOGICAL,INPUT pcEvent CHARACTER FUNCTION rowVisible,CHARACTER,INPUT pcRowids CHARACTER,INPUT phQryBuffer HANDLE FUNCTION getVisibleRowReset,LOGICAL, FUNCTION getVisibleRowids,CHARACTER, FUNCTION getTargetProcedure,HANDLE, FUNCTION getSearchField,CHARACTER, FUNCTION getScrollRemote,LOGICAL, FUNCTION getQueryRowObject,HANDLE, FUNCTION getNumDown,INTEGER, FUNCTION getMaxWidth,DECIMAL, FUNCTION getDataSignature,CHARACTER, FUNCTION getCalcWidth,LOGICAL, FUNCTION getBrowseHandle,HANDLE, FUNCTION getApplyExitOnAction,LOGICAL, FUNCTION getApplyActionOnExit,LOGICAL, FUNCTION getActionEvent,CHARACTER, FUNCTION colValues,CHARACTER,INPUT pcViewColList CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER     �              �             �� �  ��              ,j              |-    +   �| �  N   T� `  O   �� �   S   �� t  b           � �  � x  ? ��    iSO8859-1                                                                        �   �    X                                     �                   ��                L  �    �   ��   ��             ��  �   p      |                                                         PROGRESS                         �          �       �  X  �  @   �  �  �(      H  !       �             \                �     �	      �  
    
                  l  4             �                                                                                          �	          
  �  �	      ,  
    
                    �             �                                                                                          �	          
  \  
      �  
    
                  �  �             H                                                                                          
          
    
      �  
    
                  p  8             �                                                                                          
          
  �  .
      0  
    
                    �             �                                                                                          .
          
  `  @
      �  
    
                  �  �             L                                                                                          @
          
    U
      �  
    
                  t  <             �                                                                                          U
          
  �  k
      4  
    
                     �  	           �                                                                                          k
          
  d  y
      �                         �  �  
           P                                                                                          y
              �
      �                        x  @             �                                                                                          �
            �  �
      8  
    
                  $  �             �                                                                                          �
          
  h	  �
      �  
    
                  �  �	             T	                                                                                          �
          
  
  �
      �	  
    
                  |	  D
              
                                                                                          �
          
  �
  �
      <
                        (
  �
             �
                                                                                          �
            l  �
      �
                        �
  �             X                                                                                          �
              �
      �                        �  H                                                                                                       �
                �
      @                        ,                 �                                                                                          �
                          0�                                               4�          �  �  ` �,            
             
             
                                         
                                                                                                                                           
                                         
             
                                                        `   p   �   �   �   �   �   �   �   �           0  @  P  `  p  �  �  �  �  �      `   p   �   �   �   �   �   �   �   �          0  @  P  `  p  �  �  �  �  �                                                                                                                                     	                  
                                                                                                                                                                                                                                                                                                                                                                                                                               !                  "                                 �  �  �  �  �                             (  @  4          D             \  d  l  �  �                         �  �  �  �  �                         �  �  �    �                               D  0                         H  P  X  �  p                         �  �  �  �                             �  �  �  �  �          �             �  �  �    �                               ,                             0  <  D  L                             P  \  d  l                             p  |  �  �                             �  �  �  �                             �  �  �  �                             �  �  �  �                             �                                     $  ,  4                             8  D  L  \                             `  h  p  |  t          �             �  �  �  �  �          �             �  �  �    �                               <  $                         @  H  P  �  h                         �  �  �  �  �                         �  �  �  �  �                         �  �  �                                                                    $  0  8  D                              H  P  X  `                             d  p  x  �                             �  �  �  �                                                                          CodAlm  x(5)    Almac�n Almac�n     C�digo de almac�n   Descripcion X(40)   Descripci�n Descripci�n     Descripci�n de almac�n  AutMov  Si/No   Autorizacion de Movimientos Aut!Mov Si  TdoArt  Si/No   Todos los Articulos Todos los!Articulos Si  CorrIng 9999999 Correlativo Ingreso Correlativo Ingreso 0   CorrSal 9999999 Correlativo Salida  Correlativo!Salida  0   Clave   X(8)    Clave de Modificacion   Clave de!Modificacion       flgrep  yes/no  flgrep  no  CodCia  999 Cia Cia 0   C�digo de Compa�ia  AlmCsg  yes/no  Almac�n de Consignaci�n AlmCsg  no  Campo-C1    X(10)   Clasificaci�n       Campo-C2    X(10)   Campo-C     Campo-C3    X(10)   Campo-C     Campo-C4    X(10)   Campo-C     Campo-C5    X(10)   Localizaci�n        Campo-C6    X(10)   Campo-C     Campo-C7    X(10)   Campo-C     Campo-C8    X(10)   Campo-C     Campo-C9    X(10)   Campo-C     Campo-C10   X(10)   Tpo Servicio        CodCli  x(11)   RUC CodCli      Codigo del Cliente  CodDiv  x(5)    Local   Divisionaria    00000   Codigo de Divisionaria  DirAlm  X(60)   Direccion   Direccion       EncAlm  X(30)   Encargado   Encargado del Almacen       HorRec  X(40)   Horario de Recepcion    Horario de Recepcion        TelAlm  X(13)   Tel�fono    Telefono!Almacen        TpoCsg  X(1)    Tipo Csg.   TpoCsg      Campo-Log1  yes/no  Campo-Log   no  RowNum  ->,>>>,>>9  RowNum  0   RowIdent    x(8)    RowIdent        RowMod  x(8)    RowMod      RowIdentIdx x(8)    RowIdentIdx     RowUserProp x(8)    RowUserProp     �  ! 6�  ���"������                   00000               p        �        �                �     i     i     i     	  	! 	    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �        !  *  4  ;  B  I  P  W  ^  e  p  w  �  �  �    ��                                               �                                       ����                            �    T�  2                 O�    undefined                                                               �       X�  �   l   h�                        �����               h�<                    O   ����    e�          O   ����    R�          O   ����    ��      �
               assignFocusedWidget         �       �             LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �             P           LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList   0      �      �    *       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget �            H    @       LOGICAL,INPUT pcNameList CHARACTER  clearWidget (      l      �    L       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  x      �      �    X       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �      0      `    k       LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton   @      �      �    y       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    �      �      (    �       LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue          L      �  	  �       CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    d      �      �  
  �       CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      (      T   
 �       LOGICAL,INPUT pcNameList CHARACTER  highlightWidget 4      x      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    �      �           �       LOGICAL,INPUT pcNameList CHARACTER  toggleWidget           D      t    �       LOGICAL,INPUT pcNameList CHARACTER  viewWidget  T      �      �   
 �       LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    �      �          
      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �      8      l          LONGCHAR,INPUT pcName CHARACTER widgetIsBlank   L      �      �    +      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused �      �          9      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      0      d    I      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    D      �      �    Z      LOGICAL,INPUT pcName CHARACTER  widgetValue �      �      	    g      CHARACTER,INPUT pcName CHARACTER    widgetValueList �      (	      X	    s      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �                                                                                (                      4                      @                      L                      X                      d        	       	           � ߱            $   �����	   ��	  	                      ؒ    u    �      p       4   ����p                 �                      ��                  u  y                  �L>                       u     �  	  v  �                                        3   �����       O   x  ��  ��  �   addRecord                               �  p      ��                  4  5  �              (b>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            applyCellEntry                              �  t      ��                  7  9  �              �d>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            applyEntry                              �  �      ��                  ;  =  �              dE>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            assignMaxGuess                              �  �      ��                  ?  A  �              tM>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            calcWidth                                 �      ��                  C  D                 f>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                  �      ��                  F  G  $              �h>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            copyRecord                                �      ��                  I  J  $              �o>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            defaultAction                                 �      ��                  L  M  (              @p>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deleteComplete                                �      ��                  O  P  ,              �p>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deleteRecord                                         ��                  R  S  0              �s>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                                       ��                  U  V  4              |t>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableFields                                        ��                  X  Z  8              ��>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  P           ��                            ����                            displayFields                               L  4      ��                  \  ^  d              ��>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            enableFields                                x  `      ��                  `  a  �              ��>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            fetchDataSet                                |  d      ��                  c  e  �              h�>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            filterActive                                �  �      ��                  g  i  �              ��>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            initializeObject                                �  �      ��                  k  l  �              ��>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            offEnd                              �  �      ��                  n  o  �              d�>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            offHome                             �  �      ��                  q  r  �              �>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            refreshBrowse                               �   �       ��                  t  u  �               ��>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            resetRecord                             �!  �!      ��                  w  x  �!              H�>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            resizeBrowse                                �"  �"      ��                  z  }  �"              ��>                    O   ����    e�          O   ����    R�          O   ����    ��            ��   <#             #               ��                  0#           ��                            ����                            resizeObject                                ,$  $      ��                    �  D$              ��>                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �$             \$               ��                  �$           ��                            ����                            rowDisplay                              |%  d%      ��                  �  �  �%              ��>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            searchTrigger                               �&  h&      ��                  �  �  �&              (�>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            setDown                             |'  d'      ��                  �  �  �'              ��>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �'           ��                            ����                            toolbar                             �(  �(      ��                  �  �  �(              $�>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �(           ��                            ����                            updateRecord                                �)  �)      ��                  �  �  �)              �>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �*  �*      ��                  �  �  �*              ��>                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �*           ��                            ����                            updateTitle                             �+  �+      ��                  �  �  ,              �>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewObject                              �,  �,      ��                  �  �  -              X�>                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            colValues   8	      d-      �-   	 w      CHARACTER,INPUT pcViewColList CHARACTER getActionEvent  p-      �-      �-    �      CHARACTER,  getApplyActionOnExit    �-      �-      ,.    �      LOGICAL,    getApplyExitOnAction    .      8.      p.    �      LOGICAL,    getBrowseHandle P.      |.      �.    �      HANDLE, getCalcWidth    �.      �.      �.    �      LOGICAL,    getDataSignature    �.      �.      $/    �      CHARACTER,  getMaxWidth /      0/      \/    �      DECIMAL,    getNumDown  </      h/      �/    
 �      INTEGER,    getQueryRowObject   t/      �/      �/  !  �      HANDLE, getScrollRemote �/      �/      0  "        LOGICAL,    getSearchField  �/      0      H0  #  !      CHARACTER,  getTargetProcedure  (0      T0      �0  $  0      HANDLE, getVisibleRowids    h0      �0      �0  %  C      CHARACTER,  getVisibleRowReset  �0      �0      1  &  T      LOGICAL,    rowVisible  �0      1      <1  ' 
 g      CHARACTER,INPUT pcRowids CHARACTER,INPUT phQryBuffer HANDLE setActionEvent  1      x1      �1  (  r      LOGICAL,INPUT pcEvent CHARACTER setApplyActionOnExit    �1      �1       2  )  �      LOGICAL,INPUT plApply LOGICAL   setApplyExitOnAction    �1       2      X2  *  �      LOGICAL,INPUT plApply LOGICAL   setCalcWidth    82      x2      �2  +  �      LOGICAL,INPUT plCalcWidth LOGICAL   setDataModified �2      �2      �2  ,  �      LOGICAL,INPUT lModified LOGICAL setMaxWidth �2      3      H3  -  �      LOGICAL,INPUT pdMaxWidth DECIMAL    setNumDown  (3      l3      �3  . 
 �      LOGICAL,INPUT piNumDown INTEGER setQueryRowObject   x3      �3      �3  /  �      LOGICAL,INPUT phQueryRowObject HANDLE   setScrollRemote �3      4      D4  0  �      LOGICAL,INPUT plScrollRemote LOGICAL    setSearchField  $4      l4      �4  1        LOGICAL,INPUT pcField CHARACTER setVisibleRowids    |4      �4      �4  2        LOGICAL,INPUT pcRowids CHARACTER    setVisibleRowReset  �4      5      H5  3  !      LOGICAL,INPUT plReset LOGICAL   stripCalcs  (5      h5      �5  4 
 4      CHARACTER,INPUT cClause CHARACTER   getObjectType   t5      �5      �5  5  ?      CHARACTER,  addRecord                               �6  l6      ��                  �  �  �6              lP?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            cancelRecord                                �7  p7      ��                  �  �  �7              t?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            collectChanges                              �8  t8      ��                  �  �  �8              (-?                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �8             �8               ��                  �8           ��                            ����                            confirmContinue                             �9  �9      ��                  �  �  �9              �P?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  :           ��                            ����                            confirmDelete                               ;  �:      ��                  �  �  $;              ~?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  <;           ��                            ����                            confirmExit                             4<  <      ��                  �  �  L<              �1?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  d<           ��                            ����                            copyRecord                              \=  D=      ��                  �  �  t=              T6?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            dataAvailable                               `>  H>      ��                  �  �  x>              �U?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �>           ��                            ����                            deleteRecord                                �?  t?      ��                  �  �  �?               Z?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �@  |@      ��                  �  �  �@              �]?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            okToContinueProcedure                               �A  �A      ��                  �  �  �A              �^?                    O   ����    e�          O   ����    R�          O   ����    ��            ��   B             �A               ��                  �A           ��                            ����                            queryPosition                               �B  �B      ��                  �  �  C              �z?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  $C           ��                            ����                            resetRecord                             D  D      ��                  �  �  4D              |"?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            showDataMessagesProcedure                               ,E  E      ��                  �  �  DE              ,b?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \E           ��                            ����                            updateMode                              TF  <F      ��                  �  �  lF              ��?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �F           ��                            ����                            updateRecord                                �G  hG      ��                  �  �  �G              T?                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            updateState                             �H  hH      ��                  �  �  �H              �?                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �H           ��                            ����                            updateTitle                             �I  �I      ��                  �  �  �I              L5<                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            validateFields                              �J  �J      ��                  �  �  �J              8<                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �J           ��                            ����                            getCreateHandles    �5      DK      xK  6  M      CHARACTER,  getDataModified XK      �K      �K  7  ^      LOGICAL,    getDisplayedFields  �K      �K      �K  8  n      CHARACTER,  getDisplayedTables  �K       L      4L  9  �      CHARACTER,  getEnabledFields    L      @L      tL  :  �      CHARACTER,  getEnabledHandles   TL      �L      �L  ;  �      CHARACTER,  getFieldHandles �L      �L      �L  <  �      CHARACTER,  getFieldsEnabled    �L      �L      0M  =  �      LOGICAL,    getGroupAssignSource    M      <M      tM  >  �      HANDLE, getGroupAssignSourceEvents  TM      |M      �M  ?  �      CHARACTER,  getGroupAssignTarget    �M      �M      �M  @        CHARACTER,  getGroupAssignTargetEvents  �M      N      DN  A        CHARACTER,  getNewRecord    $N      PN      �N  B  8      CHARACTER,  getObjectParent `N      �N      �N  C  E      HANDLE, getRecordState  �N      �N      �N  D  U      CHARACTER,  getRowIdent �N       O      ,O  E  d      CHARACTER,  getTableIOSource    O      8O      lO  F  p      HANDLE, getTableIOSourceEvents  LO      tO      �O  G  �      CHARACTER,  getUpdateTarget �O      �O      �O  H  �      CHARACTER,  getUpdateTargetNames    �O      �O      ,P  I  �      CHARACTER,  getWindowTitleField P      8P      lP  J  �      CHARACTER,  okToContinue    LP      xP      �P  K  �      LOGICAL,INPUT pcAction CHARACTER    setContainerMode    �P      �P       Q  L  �      LOGICAL,INPUT pcContainerMode CHARACTER setDisplayedFields  �P      (Q      \Q  M  �      LOGICAL,INPUT pcFieldList CHARACTER setEnabledFields    <Q      �Q      �Q  N        LOGICAL,INPUT pcFieldList CHARACTER setGroupAssignSource    �Q      �Q      R  O        LOGICAL,INPUT phObject HANDLE   setGroupAssignSourceEvents  �Q      0R      lR  P  (      LOGICAL,INPUT pcEvents CHARACTER    setGroupAssignTarget    LR      �R      �R  Q  C      LOGICAL,INPUT pcObject CHARACTER    setGroupAssignTargetEvents  �R      �R      (S  R  X      LOGICAL,INPUT pcEvents CHARACTER    setLogicalObjectName    S      LS      �S  S  s      LOGICAL,INPUT pcLogicalObjectName CHARACTER setObjectParent dS      �S      �S  T  �      LOGICAL,INPUT phParent HANDLE   setSaveSource   �S       T      0T  U  �      LOGICAL,INPUT plSave LOGICAL    setTableIOSource    T      PT      �T  V  �      LOGICAL,INPUT phObject HANDLE   setTableIOSourceEvents  dT      �T      �T  W  �      LOGICAL,INPUT pcEvents CHARACTER    setUpdateTarget �T       U      0U  X  �      LOGICAL,INPUT pcObject CHARACTER    setUpdateTargetNames    U      TU      �U  Y  �      LOGICAL,INPUT pcTargetNames CHARACTER   setWindowTitleField lU      �U      �U  Z  �      LOGICAL,INPUT cWindowTitleField CHARACTER   showDataMessages    �U      V      HV  [        CHARACTER,  applyLayout                             �V  �V      ��                  �  �  �V               �H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �W  �W      ��                  �  �   X              ��H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �X  �X      ��                  �  �  Y              d�H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �Y  �Y      ��                  �  �  Z              ��H                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �Z  �Z      ��                  �  �  [              ��H                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  ([           ��                            ����                            getAllFieldHandles  (V      �[      �[  \        CHARACTER,  getAllFieldNames    �[      �[      \  ]  +      CHARACTER,  getCol  �[      \      8\  ^  <      DECIMAL,    getDefaultLayout    \      D\      x\  _  C      CHARACTER,  getDisableOnInit    X\      �\      �\  `  T      LOGICAL,    getEnabledObjFlds   �\      �\      �\  a  e      CHARACTER,  getEnabledObjHdls   �\      ]      8]  b  w      CHARACTER,  getHeight   ]      D]      p]  c 	 �      DECIMAL,    getHideOnInit   P]      |]      �]  d  �      LOGICAL,    getLayoutOptions    �]      �]      �]  e  �      CHARACTER,  getLayoutVariable   �]      �]      ,^  f  �      CHARACTER,  getObjectEnabled    ^      8^      l^  g  �      LOGICAL,    getObjectLayout L^      x^      �^  h  �      CHARACTER,  getRow  �^      �^      �^  i  �      DECIMAL,    getWidth    �^      �^      _  j  �      DECIMAL,    getResizeHorizontal �^       _      T_  k  �      LOGICAL,    getResizeVertical   4_      `_      �_  l  		      LOGICAL,    setAllFieldHandles  t_      �_      �_  m  	      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    �_      �_      (`  n  .	      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    `      H`      |`  o  ?	      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    \`      �`      �`  p  P	      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   �`      �`      $a  q  a	      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    a      Da      xa  r  o	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout Xa      �a      �a  s  �	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal �a      �a      $b  t  �	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   b      Pb      �b  u  �	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated db      �b      �b  v  �	      LOGICAL,    getObjectSecured    �b      �b       c  w  �	      LOGICAL,    createUiEvents   c      ,c      \c  x  �	      LOGICAL,    addLink                             �c  �c      ��                  �  �  d              ,kI                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Xd             $d  
             ��   �d             Ld               �� 
                 td  
         ��                            ����                            addMessage                              le  Te      ��                  �  �  �e              T+I                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �e             �e               ��   �e             �e               ��                  �e           ��                            ����                            adjustTabOrder                              �f  �f      ��                  �  �   g              [I                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  Lg             g  
             �� 
  tg             @g  
             ��                  hg           ��                            ����                            applyEntry                              `h  Hh      ��                  �  �  xh              ��I                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �h           ��                            ����                            changeCursor                                �i  ti      ��                  �  �  �i              �{I                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �i           ��                            ����                            createControls                              �j  �j      ��                  �  �  �j              ��I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �k  �k      ��                  �  �  �k              ,I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �l  �l      ��                      �l              <�I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �m  �m      ��                      �m              0�I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �n  �n      ��                      �n              ��I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �o  �o      ��                  
    �o              P0I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �p  �p      ��                      �p              �0I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �q  �q      ��                      �q              xDI                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  @r             r  
             ��   hr             4r               ��   �r             \r               ��                  �r           ��                            ����                            modifyUserLinks                             �s  hs      ��                      �s               RI                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �s             �s               ��   t             �s               �� 
                  t  
         ��                            ����                            removeAllLinks                              �t  �t      ��                      u              ̩I                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �u  �u      ��                     $  v              t�I                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  `v             ,v  
             ��   �v             Tv               �� 
                 |v  
         ��                            ����                            repositionObject                                |w  dw      ��                  &  )  �w              ��I                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �w             �w               ��                  �w           ��                            ����                            returnFocus                             �x  �x      ��                  +  -  �x              �I                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �x  
         ��                            ����                            showMessageProcedure                                 z  �y      ��                  /  2  z              ��I                    O   ����    e�          O   ����    R�          O   ����    ��            ��   dz             0z               ��                  Xz           ��                            ����                            toggleData                              P{  8{      ��                  4  6  h{              4�I                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �{           ��                            ����                            viewObject                              x|  `|      ��                  8  9  �|              |J                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  <c      �|      }  y 
 8      LOGICAL,    assignLinkProperty  �|       }      T}  z  C      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages   4}      �}      �}  {  V      CHARACTER,  getChildDataKey �}      �}      ~  |  d      CHARACTER,  getContainerHandle  �}      $~      X~  }  t      HANDLE, getContainerHidden  8~      `~      �~  ~  �      LOGICAL,    getContainerSource  t~      �~      �~    �      HANDLE, getContainerSourceEvents    �~      �~        �  �      CHARACTER,  getContainerType    �~      $      X  �  �      CHARACTER,  getDataLinksEnabled 8      d      �  �  �      LOGICAL,    getDataSource   x      �      �  �  �      HANDLE, getDataSourceEvents �      �      �  �  �      CHARACTER,  getDataSourceNames  �      �      P�  �        CHARACTER,  getDataTarget   0�      \�      ��  �         CHARACTER,  getDataTargetEvents l�      ��      ̀  �  .      CHARACTER,  getDBAware  ��      ؀      �  � 
 B      LOGICAL,    getDesignDataObject �      �      D�  �  M      CHARACTER,  getDynamicObject    $�      P�      ��  �  a      LOGICAL,    getInstanceProperties   d�      ��      ȁ  �  r      CHARACTER,  getLogicalObjectName    ��      ԁ      �  �  �      CHARACTER,  getLogicalVersion   �      �      L�  �  �      CHARACTER,  getObjectHidden ,�      X�      ��  �  �      LOGICAL,    getObjectInitialized    h�      ��      ̂  �  �      LOGICAL,    getObjectName   ��      ؂      �  �  �      CHARACTER,  getObjectPage   �      �      D�  �  �      INTEGER,    getObjectVersion    $�      P�      ��  �  �      CHARACTER,  getObjectVersionNumber  d�      ��      ȃ  �        CHARACTER,  getParentDataKey    ��      ԃ      �  �        CHARACTER,  getPassThroughLinks �      �      H�  �  )      CHARACTER,  getPhysicalObjectName   (�      T�      ��  �  =      CHARACTER,  getPhysicalVersion  l�      ��      ̄  �  S      CHARACTER,  getPropertyDialog   ��      ؄      �  �  f      CHARACTER,  getQueryObject  �      �      H�  �  x      LOGICAL,    getRunAttribute (�      T�      ��  �  �      CHARACTER,  getSupportedLinks   d�      ��      ą  �  �      CHARACTER,  getTranslatableProperties   ��      Ѕ      �  �  �      CHARACTER,  getUIBMode  �      �      D�  � 
 �      CHARACTER,  getUserProperty $�      P�      ��  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    `�      ��      ��  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles ��      �      4�  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    �      X�      ��  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry h�      ć      ��  �        CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   Ї      \�      ��  �        CHARACTER,INPUT piMessage INTEGER   propertyType    l�      ��      ��  �  &      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  ��      �      8�  �  3      CHARACTER,  setChildDataKey �      D�      t�  �  B      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  T�      ��      Љ  �  R      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  ��      ��      $�  �  e      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    �      D�      ��  �  x      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled `�      ��      ؊  �  �      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   ��       �      0�  �  �      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents �      P�      ��  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  d�      ��      ��  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   ��      �      8�  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents �      \�      ��  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  p�      ��      ��  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject ��       �      4�  �        LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    �      \�      ��  �        LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   p�      ��      �  �  ,      LOGICAL,INPUT pcPropList CHARACTER  setLogicalVersion   č      �      <�  �  B      LOGICAL,INPUT cVersion CHARACTER    setObjectName   �      `�      ��  �  T      LOGICAL,INPUT pcName CHARACTER  setObjectVersion    p�      ��      �  �  b      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    Ď      �      @�  �  s      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks  �      h�      ��  �  �      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   |�      ��      �  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  ԏ      �      H�  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute (�      l�      ��  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   |�      Đ      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ؐ      �      X�  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  8�      |�      ��  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty ��      ȑ      ��  �        LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ؑ      8�      d�  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   D�      ��      ��  � 	 $      CHARACTER,INPUT pcName CHARACTER    ��    O  ��  p�      �       4   �����                 ��                      ��                  P  }                  D�I                       P  �        Q  ��  �      �       4   �����                 (�                      ��                  R  |                  ȟI                       R  ��  (�    i  D�  ��      �       4   �����                 Д                      ��                  u  w                  ��I                       u  T�         v                                  �     
                    � ߱        T�  $  y  ��  ���                           $  {  ��  ���                       �                         � ߱        ��    �  ȕ  D�            4   ����                T�                      ��                  �  F	                  0�I                       �  ؕ  ��  o   �      ,                                 ��  $   �  ��  ���                       x  @         d              � ߱        ��  �   �  �      �  �   �        �  �   �  �      0�  �   �  �      D�  �   �  h      X�  �   �  �      l�  �   �  X      ��  �   �  �      ��  �   �        ��  �   �  |      ��  �   �  �      З  �   �  t      �  �   �  �      ��  �   �  ,      �  �   �  �       �  �   �  	      4�  �   �  X	      H�  �   �  �	      \�  �   �  
      p�  �   �  |
      ��  �   �  �
      ��  �   �  l      ��  �   �  �      ��  �   �  \      Ԙ  �   �  �      �  �   �  L      ��  �   �  �      �  �   �  �      $�  �   �  p      8�  �   �  �      L�  �   �         `�  �   �  \      t�  �   �  �      ��  �   �  �      ��  �   �        ��  �   �  �      ę  �   �  �      ؙ  �   �        �  �   �  @       �  �   �  |      �  �   �  �      (�  �   �  �      <�  �   �  0      P�  �   �  l          �   �  �                      |�          �  К      ��                  m	  �	   �              ��I                    O   ����    e�          O   ����    R�          O   ����    ��           
                �                     �                         � ߱        ��  $ �	  �  ���                           O   �	  ��  ��  �               �          �  �    ��                                             ��                            ����                                �5      d�      ��     M     �                      5 �  ?                     ��    �	  Ԝ  P�      �      4   �����                `�                      ��                  �	  N
                  �J                       �	  �  t�  �   �	  P      ��  �   �	  �      ��  �   �	  8      ��  �   �	  �      ĝ  �   �	  (      ؝  �   �	  �      �  �   �	         �  �   �	  �      �  �   �	         (�  �   �	  t      <�  �   �	  �      P�  �   �	  d      d�  �   �	  �      x�  �   �	  T      ��  �   �	  �      ��  �   �	  L      ��  �   �	  �      Ȟ  �   �	  D      ܞ  �   �	  �      �  �   �	  <      �  �   �	  �      �  �   �	  4      ,�  �   �	  �      @�  �   �	  ,       T�  �   �	  �       h�  �   �	  $!      |�  �   �	  �!          �   �	  "      ��    Z
  ��  (�      �"      4   �����"                8�                      ��                  [
  �
                  ��H                       [
  ��  L�  �   ]
  �"      `�  �   ^
  X#      t�  �   _
  �#      ��  �   `
  H$      ��  �   f
  �$      ��  �   g
  X%      Ġ  �   h
  �%      ؠ  �   i
  @&      �  �   j
  �&       �  �   k
  8'      �  �   l
  �'      (�  �   m
  ((      <�  �   n
  d(      P�  �   p
  �(      d�  �   q
  L)      x�  �   r
  �)      ��  �   s
  4*      ��  �   t
  �*      ��  �   u
  +      ȡ  �   v
  �+      ܡ  �   w
  ,      �  �   x
  �,      �  �   y
  �,      �  �   z
  p-      ,�  �   {
  �-      @�  �   }
   .      T�  �   ~
  �.      h�  �   �
  /      |�  �   �
  �/      ��  �   �
  �/          �   �
  t0      ��       ��  <�      �0      4   �����0                L�                      ��                    �                  �H                         Т  `�  �     1      t�  �     �1      ��  �     �1      ��  �     82      ��  �     �2      ģ  �     03      أ  �   	  �3      �  �   
   4       �  �     �4      �  �     �4      (�  �     5      <�  �     H5      P�  �     �5      d�  �     �5      x�  �     �5      ��  �     86      ��  �     t6      ��  �     �6      Ȥ  �     d7      ܤ  �     �7      �  �     \8      �  �     �8      �  �     9      ,�  �     P9      @�  �     �9      T�  �     �9      h�  �     D:      |�  �     �:      ��  �     �:      ��  �      �:      ��  �   !  4;      ̥  �   "  p;      �  �   #  �;      ��  �   $  �;      �  �   %  $<      �  �   &  `<      0�  �   '  �<      D�  �   (  �<      X�  �   )  =      l�  �   *  P=      ��  �   +  �=      ��  �   ,  �=      ��  �   -  >      ��  �   .  @>      Ц  �   /  |>          �   0  �>      getRowObject    L�  $  �   �  ���                       (?     
                    � ߱        �    �  h�  x�      <?      4   ����<?      /   �  ��     ��                          3   ����L?            ԧ                      3   ����l?  8�    �   �  |�  h�  �?      4   �����?  	              ��                      ��             	     �  z                  DF                       �  �  ��  �   �  �?      ��  $  �  ̨  ���                       @     
                    � ߱        �  �   �  4@      d�  $   �  8�  ���                       \@  @         H@              � ߱         �  $    ��  ���                       �@                         � ߱        pA     
                �A                     <C  @        
 �B              � ߱        ��  V     ��  ���                        HC                     |C       	       	       �C                         � ߱        @�  $  '  L�  ���                       xD     
                �D                     DF  @        
 F              � ߱        Ы  V   9  ܪ  ���                        PF     
                �F                     H  @        
 �G              � ߱            V   ^  l�  ���                        
              0�                      ��             
     |                    �|F                       |  ��  0H     
                �H                     �I  @        
 �I          hJ  @        
 (J          �J  @        
 �J          (K  @        
 �J              � ߱            V   �  x�  ���                        adm-clone-props \�  \�              �     N     `                          \  �                     start-super-proc    l�  ȭ  �           �     O                                  �                     Ю    3  T�  d�      �N      4   �����N      /   4  ��     ��                          3   �����N            ��                      3   �����N  ��  $  8  ��  ���                        O       
       
           � ߱        ,O     
                �O                     �P  @        
 �P              � ߱        ��  V   B  (�  ���                        ��    �  ԯ  P�      Q      4   ����Q                `�                      ��                  �  �                  lF                       �  �      g   �  x�         w�<�                           @�          �  ��      ��                  �      (�              �F                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  l�     |�  ,Q                      3   ����Q  ��     
   ��                      3   ����8Q         
   ̱                      3   ����@Q    ��                              ��                          ����                                        ��              P      ܱ                      g                               ��  g   �  ��          w�	D�                           x�          H�  0�      ��                  �  �  `�              tF                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  dQ                      3   ����HQ            Գ                      3   ����lQ    ��                              ��                          ����                                        Ĳ              Q      �                      g                               ��  g   �  ��          w�	L�                           ��          P�  8�      ��                  �  �  h�              F                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��  �Q                      3   �����Q            ܵ                      3   �����Q    ��                              ��                          ����                                        ̴              R      �                      g                               $�    �  Ķ  @�      �Q      4   �����Q                P�                      ��                  �  �                  �FF                       �  Զ  ��  /   �  |�     ��                          3   �����Q            ��                      3   �����Q      /   �  �     ��                          3   ����R  (�     
   �                      3   ����4R  X�        H�                      3   ����<R  ��        x�                      3   ����PR            ��                      3   ����lR  displayObjects  ܭ  ��                      S      �                                                    ��    �  @�  ��      �R      4   �����R                ̹                      ��                  �  �                  �OF                       �  P�  ��  /   �  ��     �                          3   �����R            (�                      3   �����R  �R     
                PS                     �T  @        
 `T              � ߱        Ⱥ  V   �  8�  ���                        Ļ  /   �  ��     �                          3   �����T  4�     
   $�                      3   �����T  d�        T�                      3   �����T  ��        ��                      3   �����T            ��                      3   ����U  ��  /   �  �      �                          3   ����,U  0�     
    �                      3   ����LU  `�        P�                      3   ����TU  ��        ��                      3   ����hU            ��                      3   �����U      /   �  �     ��                          3   �����U  ,�     
   �                      3   �����U  \�        L�                      3   �����U  ��        |�                      3   �����U            ��                      3   ���� V  t�  g   �  Խ         w4�                           ��          l�  T�      ��                  �      ��              �kF                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  Ⱦ         4V                      3   ����V    ��                            ����                                        �              T      ؾ                      g                               ,�  g   �  ��          w0��      }                      T�          $�  �      ��                  �      <�              (�F                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��         XV                      3   ����<V    ��                            ����                                        ��              U      ��                      g                               ��  $  �  X�  ���                       `V                         � ߱        H�  $  �  ��  ���                       �V                         � ߱          X�      ��  `�                      ��        0         �  �                  ��F      0W     ��     �  ��      $  �  ��  ���                       �V                         � ߱        �  $  �  ��  ���                       �V                         � ߱            4   ����W  <W                     hW                         � ߱            $  �  �  ���                       ��  $  �  ��  ���                       ,X                         � ߱        ��  $  �  �  ���                       dX                         � ߱          ��      �  ��                      ��        0         �  �                  �^F      �X     P�     �  <�      $  �  ��  ���                       �X                         � ߱        h�  $  �  <�  ���                       �X                         � ߱            4   �����X  Y                     4Y                         � ߱            $  �  x�  ���                       �Y     
                tZ                     �[  @        
 �[              � ߱        ��  V     ��  ���                        �[     
                L\                     �]  @        
 \]              � ߱        �  V   &  |�  ���                        x�    F  (�  ��      �]      4   �����]  �]     
                D^                     �_  @        
 T_              � ߱            V   P  8�  ���                                        |�          L�  4�      ��                  �  �  d�              �RF                    O   ����    e�          O   ����    R�          O   ����    ��          O   �  ��  ��  �_    ��                            ����                            ȸ  �      ��              V      ��                      
�                          �_  @         �_          �_  @         �_              � ߱        ��  $   �  0�  ���                       x�  g   �  ��          w�	�                            ��          T�  <�      ��                  �  �  l�              �rF                    O   ����    e�          O   ����    R�          O   ����    ��            �  `          ��                              ��                            ��        �                  ����                                        ��              W      ��                      g                               L�  g   �  ��          w�	��                            X�          (�  �      ��                  �  �  @�              �sF                    O   ����    e�          O   ����    R�          O   ����    ��            �   `          ��                              ��                            ��        �                  ����                                        ��              X      p�                      g                               $�  g   �  d�         w@��                            ,�          ��  ��      ��                  �    �              0tF                    O   ����    e�          O   ����    R�          O   ����    ��          /     X�         P`                      3   ����4`    ��                              ��                          ����                                        x�              Y      h�                      g                               ��  g   	  <�         wB��                            �          ��  ��      ��                  
    ��              WF                    O   ����    e�          O   ����    R�          O   ����    ��          /    0�         l`                      3   ����X`    ��                              ��                          ����                                        P�              Z      @�                      g                               ��  g     �         w x�                            ��          ��  ��      ��                    +  ��              XF                    O   ����    e�          O   ����    R�          O   ����    ��          /  (  �         �`                      3   ����t`    ��                              ��                          ����                                        (�              [      �                      g                               ��  g   2  ��         wOP�                            ��          ��  l�      ��                  3  H  ��              �G                    O   ����    e�          O   ����    R�          O   ����    ��          /  =  ��         �`                      3   �����`    ��                              ��                          ����                                         �              \      ��                      g                               ��  g   O  ��         wN(�                            ��          \�  D�      ��                  P  \  t�              ��F                    O   ����    e�          O   ����    R�          O   ����    ��          /  Z  ��         �`                      3   �����`    ��                              ��                          ����                                        ��              ]      ��                      g                               ��  g   c  ��         w~D�                            d�          4�  �      ��                  d  s  L�              `�F                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  n  ��         �`                      3   �����`        o  ��  ��      �`      4   �����`      O  p  ������   a    ��                              ��                          ����                                        ��              ^      ��                      g                               ��  g   z  ��         w`�                            ��          P�  8�      ��                  {  �  h�              G                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  �  ��         ,a                      3   ����a        �  ��  ��      4a      4   ����4a      O  �  ������  La    ��                              ��                          ����                                        ��              _       �                      g                               ��  g   �  ��         w���                             �          l�  T�      ��                 �  u  ��              	G                    O   ����    e�          O   ����    R�          O   ����    ��      `a     
                �a                     ,c  @        
 �b              � ߱        ��  V   �  ��  ���                        @c     
                �c                     �d                         � ߱        ��  $  �  ,�  ���                             �  ��  T�  ��  e      4   ����e                d�                      ��                  �                    (�F                       �  ��      /  �  ��         �e                      3   ����te          ��  8�      �e      4   �����e                ��                      ��                    m                  ��F                         ��  �e     
                f                     (g                         � ߱        <�  $    H�  ���                       hg     
                �g                     �h     
                    � ߱        h�  $  .  ��  ���                       ��  $  E  ��  ���                       4i                         � ߱            p   F  �i  ��      l  ��  X�     �i                h�                      ��                  H  T                  ��F                       H  ��      /  R  ��         �i                      3   �����i       �     �i                0�                      ��                  V  k                  X�F                       V  ��      /  `  \�         �i                      3   �����i               �          ��  ��   T ��                          
                                             $   4   D          $   4   D    �          ��                              ��                            ��        �                  ����                            ��          ��      l�     `     �                      g   �                              g   |  �         w4l�                            ��          ��  ��      ��                  }  �  ��              @�F                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��         j                      3   �����i    ��                              ��                          ����                                        �              a      �                      g                               disable_UI  ��  ��                      b                                    {  
                    �� �   ���  �         �  ��              8   ����        8   ����        ��  ��      toggleData  ,INPUT plEnabled LOGICAL    x�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  �  (�      returnFocus ,INPUT hTarget HANDLE   �  P�  d�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    @�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��   �  �      removeAllLinks  ,   ��  $�  4�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    |�  �  $�      hideObject  ,   �  8�  D�      exitObject  ,   (�  X�  p�      editInstanceProperties  ,   H�  ��  ��      displayLinks    ,   t�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  �  �      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER ��  p�  |�      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER `�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  0�  @�      processAction   ,INPUT pcAction CHARACTER    �  l�  |�      enableObject    ,   \�  ��  ��      disableObject   ,   ��  ��  ��      applyLayout ,   ��  ��  ��      validateFields  ,INPUT-OUTPUT pcNotValidFields CHARACTER    ��   �  ,�      updateMode  ,INPUT pcMode CHARACTER �  T�  p�      showDataMessagesProcedure   ,OUTPUT pcReturn CHARACTER  D�  ��  ��      queryPosition   ,INPUT pcState CHARACTER    ��  ��  ��      okToContinueProcedure   ,INPUT pcAction CHARACTER,OUTPUT plAnswer LOGICAL   ��  4�  D�      dataAvailable   ,INPUT pcRelative CHARACTER $�  p�  |�      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  `�  ��  ��      confirmDelete   ,INPUT-OUTPUT plAnswer LOGICAL  ��  ��  ��      confirmContinue ,INPUT-OUTPUT plCancel LOGICAL  ��  ,�  <�      collectChanges  ,INPUT-OUTPUT pcChanges CHARACTER,INPUT-OUTPUT pcInfo CHARACTER �  ��  ��      viewObject  ,   |�  ��  ��      updateTitle ,   ��  ��  ��      updateState ,INPUT pcState CHARACTER    ��  �  �      updateRecord    ,   ��  (�  0�      toolbar ,INPUT pcValue CHARACTER    �  \�  d�      setDown ,INPUT piNumDown INTEGER    L�  ��  ��      searchTrigger   ,   ��  ��  ��      rowDisplay  ,   ��  ��  ��      resizeObject    ,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL ��  $�  4�      resizeBrowse    ,INPUT pd_height DECIMAL,INPUT pd_width DECIMAL �  t�  ��      resetRecord ,   d�  ��  ��      refreshBrowse   ,   ��  ��  ��      offHome ,   ��  ��  ��      offEnd  ,   ��  ��  �      initializeObject    ,   ��  �  (�      filterActive    ,INPUT plActive LOGICAL �  P�  `�      fetchDataSet    ,INPUT pcState CHARACTER    @�  ��  ��      enableFields    ,   |�  ��  ��      displayFields   ,INPUT pcColValues CHARACTER    ��  ��   �      disableFields   ,INPUT pcFields CHARACTER   ��  ,�  <�      destroyObject   ,   �  P�  `�      deleteRecord    ,   @�  t�  ��      deleteComplete  ,   d�  ��  ��      defaultAction   ,   ��  ��  ��      copyRecord  ,   ��  ��  ��      cancelRecord    ,   ��   �  �      calcWidth   ,   ��   �  0�      assignMaxGuess  ,INPUT piMaxGuess INTEGER   �  \�  h�      applyEntry  ,INPUT pcField CHARACTER    L�  ��  ��      applyCellEntry  ,INPUT pcCellName CHARACTER ��  ��  ��      addRecord   ,       "       "       "       "       "       "       "       "       "   	     �     }        ��   F   %               � 
"    
 %              � ��  �         �      \     H     $              
�    � .        
�             �G� .   �G     
�             �G                      
�            � 0     
"    
 Y
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        X    7%               
"   
 ��           �    1� @  
 �� K   %               o%   o           � P    �
"   
 ��                1� Q   �� K   %               o%   o           � _   �
"   
 ��           t    1� f  
 �� K   %               o%   o           � q   �
"   
 ��           �    1� �   �� K   %               o%   o           � P    �
"   
 ��           \    1� �   �� K   %               o%   o           � �   �
"   
 ��           �    1� �   �� �   %               o%   o           %               
"   
 �          L    1� �   � �     
"   
 ��           �    1� �   �� K   %               o%   o           � �  � �
"   
 ��           �    1� �   �� K   %               o%   o           � �  ( �
"   
 ��           p    1� �   �� �   %               o%   o           %               
"   
 ��           �    1� �   �� �   %               o%   o           %               
"   
 ��           h    1� �   �� �   %               o%   o           %              
"   
 �          �    1�    � �     
"   
 ��                1�   
 �� �   %               o%   o           %               
"   
 ��           �    1� &   �� K   %               o%   o           � P    �
"   
 �          	    1� .   � �     
"   
 ��           L	    1� >   �� K   %               o%   o           � T  t �
"   
 �          �	    1� �  
 � �     
"   
 ��           �	    1� �   �� K   %               o%   o           � �  � �
"   
 ��           p
    1� r   �� K   %               o%   o           � P    �
"   
 ��           �
    1� �  
 �� �   %               o%   o           %               
"   
 I�           `    1� �   I� �   %               o%   o           %               
"   
 I�           �    1� �   I� K   %               o%   o           � P    I
"   
 I�           P    1� �   I� K   %               o%   o           o%   o           
"   
 I�           �    1� �  
 I� K   %               o%   o           � P    I
"   
 I�           @    1� �   I� �  	 %               o%   o           � �  / I
"   
 �          �    1�    � �  	   
"   
 I�           �    1� )   I� �  	 o%   o           o%   o           � P    I
"   
 �          d    1� <   � �  	   
"   
 I�           �    1� K   I� �  	 o%   o           o%   o           � P    I
"   
 �              1� [   � �     
"   
 �          P    1� i   � �  	   
"   
 �          �    1� v   � �  	   
"   
 �          �    1� �   � �  	   
"   
 I�               1� �   I� �   o%   o           o%   o           %              
"   
 �          �    1� �   � �  	   
"   
 �          �    1� �  
 � �     
"   
 �          �    1� �   � �  	   
"   
 �          4    1� �   � �  	   
"   
 �          p    1� �   � �  	   
"   
 �          �    1� �   � �  	   
"   
 �          �    1� 	  	 � �  	   
"   
 �          $    1�    � �  	   
"   
 �          `    1� &   � �  	   
"   
 I�           �    1� =   I� K   %               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 F
"   
   
"   
 Y(�  L ( l       �        d    �� I   � P   �        p    �@    
� @  , 
�       |    �� R     p�               �L
�    %              � 8      �    � $         � Y          
�    � s     
"   
 �� @  , 
�       �    �� f  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 F�           D    1� v   F� �  	 %               o%   o           � P    F
"   
 F�           �    1� �   F� �  	 %               o%   o           � P    F
"   
 F�           ,    1� �   F� �   %               o%   o           %               
"   
 I�           �    1� �   I� �  	 %               o%   o           � P    F
"   
 I�               1� �   I� �  	 %               o%   o           � P    I
"   
 <�           �    1� �   <� �   %               o%   o           %               
"   
 I�               1� �   I� �  	 %               o%   o           � P    <
"   
 F�           �    1� �   F� �  	 %               o%   o           � P    I
"   
 F�           �    1� �   F� �  	 %               o%   o           � P    F
"   
 F�           h    1� �   F� �  	 %               o%   o           o%   o           
"   
 F�           �    1�    F� �  	 %               o%   o           � P    F
"   
 I�           X    1�    I� �  	 %               o%   o           � P    F
"   
 I�           �    1� "  	 I� �   %               o%   o           %               
"   
 <�           H    1� ,   <� �   %               o%   o           %               
"   
 <�           �    1� 5   <� �   %               o%   o           o%   o           
"   
 I�           @    1� F   I� �   %               o%   o           o%   o           
"   
 F�           �    1� U   F� �   %               o%   o           %               
"   
 F�           8    1� c   F� �   %               o%   o           %               
"   
 F�           �    1� t   F� �   %               o%   o           %               
"   
 I�           0    1� �   I� �   %               o%   o           %       
       
"   
 I�           �    1� �   I� �   %               o%   o           o%   o           
"   
 F�           (    1� �   F� �   %               o%   o           %              
"   
 F�           �    1� �   F� �   %               o%   o           o%   o           
"   
 I�                 1� �   I� �   %               o%   o           %              
"   
 I�           �     1� �   I� �   %               o%   o           o%   o           
"   
 F�           !    1� �   F� �   %               o%   o           %              
"   
 F�           �!    1� �   F� �   %               o%   o           o%   o           
"   
 I�           "    1� �   I� �  	 %               o%   o           � P    FP �L 
�H T   %              �     }        �GG %              
"   
 I�           �"    1� �   I� K   %               o%   o           � P    I
"   
 F�           L#    1�    F� �   %               o%   o           %               
"   
 <�           �#    1�    <� K   %               o%   o           � P    F
"   
 F�     ,      <$    1� (   F� K   %               o%   o           �   � .     � 8   Y� :  	 F
"   
 <�           �$    1� D   <� �   %               o%   o           o%   o           
"   
 F�           L%    1� M   F� K   %               o%   o           � P    I
"   
 F�           �%    1� [   F� K   %               o%   o           � P    F
"   
 F�           4&    1� j   F� �  	 %               o%   o           o%   o           
"   
 I�           �&    1� �   I� K   %               o%   o           o%   o           
"   
 F�           ,'    1� �   F� K   %               o%   o           � P    I
"   
 I�           �'    1� �   I� �   %               o%   o           %               
"   
 �          (    1� �   � �     
"   
 I�           X(    1� �   I� K   %               o%   o           � �  ~ <
"   
 I�           �(    1� U   I� K   %               o%   o           � P    I
"   
 F�           @)    1� g   F� K   %               o%   o           �    I
"   
 I�           �)    1� �   I� �  	 %               o%   o           � �   F
"   
 I�           (*    1� �   I� �  	 %               o%   o           � �   I
"   
 F�           �*    1� �  	 F� K   %               o%   o           � �   I
"   
 I�           +    1� �  
 I� �  	 %               o%   o           � �   F
"   
 I�           �+    1� �   I� �   %               o%   o           o%   o           
"   
 I�            ,    1� �   I� K   %               o%   o           �    <
"   
 I�           t,    1� w   I� K   %               o%   o           � P    I
"   
 I�           �,    1�   
 I� �   %               o%   o           o%   o           
"   
 �          d-    1� #   � �     
"   
 I�           �-    1� 1   I� K   %               o%   o           � E  ] I
"   
 F�           .    1� �   F� K   %               o%   o           � P    I
"   
 I�           �.    1� �   I� K   %               o%   o           � �   F
"   
 <�           �.    1� �   <� �   %               o%   o           %               
"   
 F�           x/    1� �   F� K   %               o%   o           � P    <
"   
 F�           �/    1� �   F� K   %               o%   o           o%   o           
"   
 �          h0    1� �   � �  	   P �L 
�H T   %              �     }        �GG %              
"   
 F�           �0    1�    F� �   %               o%   o           o%   o           
"   
 �          t1    1�    � �     
"   
 F�           �1    1� #   F� �   %               o%   o           %               
"   
 I�           ,2    1� 1  	 I� �   %               o%   o           %               
"   
 F�           �2    1� ;   F� �   %               o%   o           %       P       
"   
 I�           $3    1� D   I� K   %               o%   o           � P    F
"   
 F�           �3    1� S   F� �   %               o%   o           %               
"   
 I�           4    1� [   I� K   %               o%   o           � P    F
"   
 �          �4    1� g   � �     
"   
 �          �4    1� t   � K     
"   
 �           5    1� �   � �     
"   
 �          <5    1� �   � �     
"   
 �          x5    1� �   � �     
"   
 �          �5    1� �   � �     
"   
 �          �5    1� �   � K     
"   
 �          ,6    1� �   � �     
"   
 F�           h6    1� �   F� K   %               o%   o           � P    F
"   
 F�           �6    1� �   F� �   %               o%   o           %              
"   
 I�           X7    1� 	   I� �   %               o%   o           %              
"   
 I�           �7    1�    I� �   %               o%   o           %               
"   
 I�           P8    1� $   I� �   %               o%   o           %               
"   
 �          �8    1� 4   � �     
"   
 �          9    1� B   � �     
"   
 �          D9    1� Q   � K     
"   
 �          �9    1� a   � K     
"   
 F�           �9    1� s  
 F� �   %               o%   o           %              
"   
 �          8:    1� ~   � K     
"   
 �          t:    1� �   � K     
"   
 �          �:    1� �   � K     
"   
 �          �:    1� �   � K     
"   
 �          (;    1� �   � K     
"   
 �          d;    1� �   � K     
"   
 �          �;    1�    � K     
"   
 �          �;    1�    � �  	   
"   
 �          <    1� -   � �  	   
"   
 �          T<    1� ?   � �  	   
"   
 �          �<    1� Q   � �  	   
"   
 �          �<    1� h   � �  	   
"   
 �          =    1� z   � �  	   
"   
 �          D=    1� �   � �  	   
"   
 �          �=    1� �   � �  	   
"   
 �          �=    1� �   � �  	   
"   
 �          �=    1� �   � �  	   
"   
 �          4>    1� �   � �  	   
"   
 �          p>    1�    � �  	   
"   
 <�           �>    1�    <� �   %               o%   o           %              
�             �G "    %     start-super-proc A%     adm2/smart.p wYP �L 
�H T   %              �     }        �GG %              
"   
   �       �?    6� I     
"   
   
�        @    8
"   
   �        (@    ��     }        �G 4              
"   
 ߱G %              G %              %� � �   ScrollRemote,NumDown,CalcWidth,MaxWidth,FetchOnReposToEnd,UseSortIndicator,SearchField,DataSourceNames,UpdateTargetNames,LogicalObjectName,HideOnInit,DisableOnInit,ObjectLayout wY
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
   (�  L ( l       �        �A    �� I   � P   �        �A    �@    
� @  , 
�       �A    �� R   Yp�               �L
�    %              � 8      �A    � $         � Y          
�    � s   Y
"   
 �p� @  , 
�       �B    �� �   �p�               �L"    , �   � I   I� K   �     }        �A      |    "      � I   F%              (<   \ (    |    �     }        �A� M   �A"  	  I    "    Y"  	  I  < "    Y"  	  I(    |    �     }        �A� M   �A"  	  I
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
   (�  L ( l       �        �D    �� I   � P   �        �D    �@    
� @  , 
�       �D    �� R   Yp�               �L
�    %              � 8      �D    � $         � Y          
�    � s   Y
"   
 �p� @  , 
�       �E    �� @  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
 I(�  L ( l       �        �F    �� I   � P   �        �F    �@    
� @  , 
�       �F    �� R   Yp�               �L
�    %              � 8      �F    � $         � Y   Y     
�    � s   
"   
 �p� @  , 
�       �G    �� �   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
   
"   
 F
"   
   
"   
   (�  L ( l       �        |H    �� I   � P   �        �H    �@    
� @  , 
�       �H    �� R     p�               �L
�    %              � 8      �H    � $         � Y          
�    � s     
"   
 �p� @  , 
�       �I    �� f  
 �p�               �L%     SmartDataBrowser  �
"   
   p� @  , 
�       J    �� �     p�               �L%               
"   
  p� @  , 
�       |J    �� K    p�               �L%               
"   
  p� @  , 
�       �J    �� )    p�               �L(        � P      � P      � P      �     }        �A
�H T   %              �     }        �GG %              
"   
 < (   � 
"   
 Y    �        �K    �� I   �
"   
   � 8      L    � $         � Y          
�    � s   Y
"   
   �        `L    �
"   
   �       �L    /
"   
   
"   
   �       �L    6� I     
"   
   
�        �L    8
"   
   �        �L    �
"   
   �       M    �
"   
   p�    � v   F
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 Y    �        �M    �A"    �A
"   
   
�        (N    �@ � 
"   
 <"      �       }        �
"   
 %              %                "    %     start-super-proc @%     adm2/visual.p Y�   � .     � 8     � �     
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
   (�  L ( l       �        xO    �� I   � P   �        �O    �@    
� @  , 
�       �O    �� R   Yp�               �L
�    %              � 8      �O    � $         � Y          
�    � s   Y
"   
 �p� @  , 
�       �P    �� �   �p�               �L"  
  , � 
"    
 %     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP wY%     processAction   
�    %     CTRL-PAGE-DOWN  "    %     start-super-proc ?%     adm2/datavis.p %     modifyListProperty 
�    %      ADD     %     SupportedLinks %     Toolbar-Target  "    %     start-super-proc >%     adm2/browser.p 
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
 F(�  L ( l       �         S    �� I   � P   �        ,S    �@    
� @  , 
�       8S    �� R   Yp�               �L
�    %              � 8      DS    � $         � Y   Y     
�    � s   
"   
 �p� @  , 
�       TT    ��    �p�               �L
�             �G%     modifyListProperty 
�    %      ADD     %     DataSourceEvents �I%     FilterActive nts%     modifyListProperty 
�    %      ADD     %     DataSourceEvents �I%     RefreshBrowse ts%     modifyListProperty 
�    %      ADD     %     DataSourceEvents �I% 	    CancelNew Ev%     valueChanged    
�    %     valueChanged    
�    �,            $     � �  F ߱        � 	  
 Y�    "      � 8         %              %                   "      %                  "      "      "     T   "    Y"    F� 8    T h     @   "      (        "    � P      � .   Y� P    F(  4  8    "    �   
 FT   %              "    F� M   "      �,            $     � �  F ߱        � 	  
 Y�    "      � 8         %              %                   "      %                  "      "      "     T   "    Y"    F� 8    T h     @   "      (        "    � P      � .   Y� P    F(  4  8    "    �   
 FT   %              "    F� M   "      
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
   (�  L ( l       �        DZ    �� I   � P   �        PZ    �@    
� @  , 
�       \Z    �� R   Yp�               �L
�    %              � 8      hZ    � $         � Y          
�    � s   Y
"   
 �p� @  , 
�       x[    ��    �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
   (�  L ( l       �        \    �� I   � P   �        (\    �@    
� @  , 
�       4\    �� R   Yp�               �L
�    %              � 8      @\    � $         � Y          
�    � s   Y
"   
 �p� @  , 
�       P]    �� M   �p�               �L"    ,     "    F� P    
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
 Y(�  L ( l       �        ^    �� I   � P   �         ^    �@    
� @  , 
�       ,^    �� R   Yp�               �L
�    %              � 8      8^    � $         � Y   Y     
�    � s   Y
"   
 �p� @  , 
�       H_    �� �   �p�               �L%              
�     
         �G�             I%               �             �%              %      END     %      HOME    %     defaultAction   
�    %      onEnd   
�    %      onHome  
�    %      offEnd  
�    %      offHome 
�    %     rowEntry wY
�        �  � 5  	 Y%               %     rowLeave wY
�        �  � 5  	 Y%               
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
 Y(�  L ( l       �        �a    �� I   � P   �        �a    �@    
� @  , 
�       �a    �� R   Yp�               �L
�    %              � 8      �a    � $         � Y   Y     
�    � s   Y
"   
 �p� @  , 
�       �b    �� �   �p�               �L%              
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
 (�  L ( l       �        �c    �� I   � P   �        �c    �@    
� @  , 
�       �c    �� R   Yp�               �L
�    %              � 8      �c    � $         � Y        
�    � s     
"   
 �� @  , 
�       �d    �� �   �p�               �L0 0       �             �%                   �             ��             <%      offEnd  
�    "      
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
 (�  L ( l       �        �e    �� I   � P   �        �e    �@    
� @  , 
�        f    �� R   Yp�               �L
�    %              � 8      f    � $         � Y        
�    � s     
"   
 �� @  , 
�       g    �� �   �p�               �L
�H T   %              �     }        �GG %              
"   
 Y
"   
 
"   
 Y
"   
 (�  L ( l       �        �g    �� I   � P   �        �g    �@    
� @  , 
�       �g    �� R   Yp�               �L
�    %              � 8      �g    � $         � Y        
�    � s     
"   
 �
� @  , 
�       �h    �� �   �p�               �L�P            $     "    ߱                $     
"   
 Y        � e  
 F"      � p     %      offHome 
�    � v     %      offEnd  
�    %     onValueChanged  
�    �     }        �
�                    �           �   l       ��                 �  �  �               8F                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       pK     
                    � ߱              �  (  �      �K      4   �����K                �                      ��                  �  �                  �2F                       �  8  �  �  �  L            �  �  `      lL      4   ����lL                p                      ��                  �  �                  D3F                       �  �  �  o   �      ,                                 �  �   �  �L      �  �   �  �L      $  $  �  �  ���                       �L     
                    � ߱        8  �   �  M      L  �   �  $M      `  �   �  DM          $   �  �  ���                       tM  @         `M              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �    �               �F                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          �  $  �    ���                       �M     
                    � ߱                  �  �                      ��                   �  �                  �fF                     �  4      4   �����M      $  �  �  ���                       4N     
                    � ߱        �    �  4  D      HN      4   ����HN      /  �  p                               3   ����\N  �  �     hN          O     ��  ��  �N                               , �                          
                               �      ��                            ����                                                        �   l       ��                  M  n  �               pGF                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                  �  �  �               ��F                    O   ����    e�          O   ����    R�          O   ����    ��      �      �  �� �                       �  �         j      4   ����j      �   �  $j    ��                              ��                          ����                                �0�          �  �
  	 �                  �
          
 �                                                                �  �     �         �                                   g     �      
 �                                                               �  !     �  
     �                    "       �  �  �  �  g     �      
 �                                                               �  �     �  (     ��                                   g     �      
 �                                                               �  �     �       ��                                   g     �      
 �                                                               �  �     �         �                                   g     �      
 �                                                               �  �     �       ��    (                                g     �      
 �                                                               �  �     �       ��    (                                g     �      
 �                                                               �  �     �       ��                                    g     �      
 �                                                                �  �     �       �                                     �                                                                                                                                       �   d d     t   ���0  �0  � �                                                                                                                        d     D                                                                 H  d d �0�                                 �          �            D                                                                    TXS ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST RowObject CodAlm Descripcion AutMov TdoArt CorrIng CorrSal Clave flgrep CodCia AlmCsg Campo-C1 Campo-C2 Campo-C3 Campo-C4 Campo-C5 Campo-C6 Campo-C7 Campo-C8 Campo-C9 Campo-C10 CodCli CodDiv DirAlm EncAlm HorRec TelAlm TpoCsg Campo-Log1 RowNum RowIdent RowMod RowIdentIdx RowUserProp br_table x(5) Activo  Inactivo I X(10) X(40) Si/No 9999999 X(8) yes/no C�digo de almac�n ? Descripci�n de almac�n F-Main D:\newsie\on_in_co\aplic\alm\balmacen.w should only be RUN PERSISTENT. glReposition cLastEvent COLVALUES GETACTIONEVENT GETAPPLYACTIONONEXIT GETAPPLYEXITONACTION GETBROWSEHANDLE GETCALCWIDTH GETDATASIGNATURE GETMAXWIDTH GETNUMDOWN GETQUERYROWOBJECT GETSCROLLREMOTE GETSEARCHFIELD GETTARGETPROCEDURE GETVISIBLEROWIDS GETVISIBLEROWRESET ROWVISIBLE SETACTIONEVENT SETAPPLYACTIONONEXIT SETAPPLYEXITONACTION SETCALCWIDTH SETDATAMODIFIED SETMAXWIDTH SETNUMDOWN SETQUERYROWOBJECT SETSCROLLREMOTE SETSEARCHFIELD SETVISIBLEROWIDS SETVISIBLEROWRESET STRIPCALCS GETOBJECTTYPE GETCREATEHANDLES GETDATAMODIFIED GETDISPLAYEDFIELDS GETDISPLAYEDTABLES GETENABLEDFIELDS GETENABLEDHANDLES GETFIELDHANDLES GETFIELDSENABLED GETGROUPASSIGNSOURCE GETGROUPASSIGNSOURCEEVENTS GETGROUPASSIGNTARGET GETGROUPASSIGNTARGETEVENTS GETNEWRECORD GETOBJECTPARENT GETRECORDSTATE GETROWIDENT GETTABLEIOSOURCE GETTABLEIOSOURCEEVENTS GETUPDATETARGET GETUPDATETARGETNAMES GETWINDOWTITLEFIELD OKTOCONTINUE SETCONTAINERMODE SETDISPLAYEDFIELDS SETENABLEDFIELDS SETGROUPASSIGNSOURCE SETGROUPASSIGNSOURCEEVENTS SETGROUPASSIGNTARGET SETGROUPASSIGNTARGETEVENTS SETLOGICALOBJECTNAME SETOBJECTPARENT SETSAVESOURCE SETTABLEIOSOURCE SETTABLEIOSOURCEEVENTS SETUPDATETARGET SETUPDATETARGETNAMES SETWINDOWTITLEFIELD SHOWDATAMESSAGES GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALVERSION SETOBJECTNAME SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartDataBrowser ContainerType PropertyDialog adm2/support/browsed.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties ScrollRemote,NumDown,CalcWidth,MaxWidth,FetchOnReposToEnd,UseSortIndicator,SearchField,DataSourceNames,UpdateTargetNames,LogicalObjectName,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks TableIO-Target,Data-Target,Update-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CreateHandles DataModified DisplayedFields DisplayedTables   rowObject Editable EnabledFields EnabledHandles EnabledObjFldsToDisable EnabledWhenNew FieldHandles FieldsEnabled GroupAssignSource GroupAssignSourceEvents addRecord,copyRecord,updateRecord,resetRecord,undoRecord,cancelRecord,enableFields,disableFields,collectChanges,validateFields GroupAssignTarget GroupAssignTargetEvents updateState,LinkState InternalDisplayFromSource (Large) ModifyFields (All) NewRecord No ObjectMode View PrintPreviewActive RecordState NoRecordAvailable SaveSource TableIOSource TableIOSourceEvents addRecord,updateRecord,copyRecord,deleteRecord,resetRecord,undoChange,cancelRecord,updateMode ToolbarSource ToolbarSourceEvents toolbar UndoNew UpdateTarget UpdateTargetNames WindowTitleField SeparatorFGColor BrowseHandle BrowseInitted CalcWidth MaxWidth ModifiedFields NumDown SearchField SearchHandle ActionEvent ApplyActionOnExit LOG ApplyExitOnAction ScrollRemote QueryRowObject VisibleRowids VisibleRowReset FolderWindowToLaunch FetchOnReposToEnd PopupActive ColumnsMovable ColumnsSortable MovableHandle SortableHandle SavedColumnData DefaultColumnData Separators BrowseColumnBGColors BrowseColumnFGColors BrowseColumnLabelBGColors BrowseColumnLabelFGColors BrowseColumnLabelFonts BrowseColumnLabels BrowseColumnWidths BrowseColumnFormats BrowseColumnFonts BrowseColumnTypes BrowseColumnDelimiters BrowseColumnItems BrowseColumnItemPairs BrowseColumnInnerLines BrowseColumnSorts BrowseColumnMaxChars BrowseColumnAutoCompletions BrowseColumnUniqueMatches Tooltip UseSortIndicator ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cFields adm2/visual.p CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/datavis.p ADD Toolbar-Target DISPLAYOBJECTS cViewCols cEnabled iCol iEntries cEntry cBaseQuery hQuery cColumns iTable hColumn lResult cStripDisp cStripEnable adm2/browser.p FilterActive RefreshBrowse CancelNew CodAlm Campo-C9 Descripcion AutMov TdoArt CorrIng CorrSal Clave flgrep stripCalcs RowObject. GETROWOBJECT END HOME adm-error cRowVis hRowObj lScrollRemote cRowids rowVisible FIRST LAST DISABLE_UI Almac�n Estado Descripci�n Autoriza!Despacho Asignaci�n!Autom�tica Correlativo!Ingreso Correlativo!Salida Clave de!Modificacion Propio �  h%  �  l-      . �    ��      0         pcCellName      ��      T         pcField     ��      t         piMaxGuess      ��      �         pcFields        ��      �         pcColValues     ��      �         pcState     ��               plActive    0  ��      $        pd_height       ��      H        pd_height   x  ��      l        pd_height       ��      �        pd_height       ��      �        piNumDown       ��      �        pcValue     ��      �        pcState $  ��              pcChanges       ��      <        pcChanges       ��      `        plCancel        ��      �        plAnswer        ��      �        plCancel        ��      �        pcRelative  �  ��      �        pcAction        ��              pcAction        ��      8        pcState     ��      X        pcReturn        ��      |        pcMode      ��      �        pcState     ��      �        pcNotValidFields        ��      �        pcAction      ��             
 phSource    <  ��      0        phSource        ��      T       
 phSource    �  ��      x        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��      �       
 phObject        ��               phObject        ��      D        pcField     ��      d        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller       ��              pcMod   @  ��      8        pcMod       ��      X       
 pcMod   �  ��      x       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      $       
 hTarget P  ��      D        pcMessage       ��      h        pcMessage       ��      �        plEnabled             �     cType       �     M   �          �                  getObjectType   �	  �	  �	  $          
   hReposBuffer    D        8  
   hPropTable  `        X  
   hBuffer           t  
   hTable  �  �     N              �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �            	  
   hProc             4	        pcProcName  |  �	  	   O    	  	      p	                  start-super-proc    �  �  �  �  �  �        @	  �	     P                                   �  �	  
     Q                                   �  �  �	  D
     R                                   �  �  
  �
     S               |
                  displayObjects  n  L
  �
     T                                   �  �
  �
     U                                   �  �
  8     V               (                  getRowObject    �  �  �
  p     W                                   �  �  @  �     X                                   �  �  x  �     Y                                        �       Z                                       �  P     [                                   (  +     �     \                                   =  H  X  �     ]                                   Z  \  �  �     ^                                   n  o  p  s  �  8     _                                   �  �  �  �  d        \     cRowVis �        x  
   hRowObj �        �     lScrollRemote             �     cRowids   �     `   H                              �  �  �  �  �        .  E  F  H  R  T  V  `  k  l  m  u  �  p     a                                   �  �  @  �     b               �                  disable_UI  �  �  �  x  �  �      �      l                               !   RowObject   �         �         �         �         �         �         �         �         �         �         �         �                                    (         4         @         L         X         d         l         t         |         �         �         �         �         �         �         �         �         �         CodAlm  Descripcion AutMov  TdoArt  CorrIng CorrSal Clave   flgrep  CodCia  AlmCsg  Campo-C1    Campo-C2    Campo-C3    Campo-C4    Campo-C5    Campo-C6    Campo-C7    Campo-C8    Campo-C9    Campo-C10   CodCli  CodDiv  DirAlm  EncAlm  HorRec  TelAlm  TpoCsg  Campo-Log1  RowNum  RowIdent    RowMod  RowIdentIdx RowUserProp         �     glReposition                 cLastEvent  H        4  
   gshAstraAppserver   p        \  
   gshSessionManager   �        �  
   gshRIManager    �        �  
   gshSecurityManager  �        �  
   gshProfileManager           �  
   gshRepositoryManager    <        $  
   gshTranslationManager   `  	 	     P  
   gshWebManager   �  
 
     t     gscSessionId    �        �     gsdSessionObj   �        �  
   gshFinManager   �        �  
   gshGenManager             
   gshAgnManager   8        (     gsdTempUniqueID X        L     gsdUserObj  �        l     gsdRenderTypeObj    �        �     gsdSessionScopeObj  �       �  
   ghProp  �       �  
   ghADMProps         �  
   ghADMPropsBuf   0            glADMLoadFromRepos  L       D     glADMOk l       `  
   ghContainer �       �     cObjectName �    	   �     iStart  �    
   �     cFields �       �     cViewCols          �     cEnabled                 iCol    @       4     iEntries    \       T     cEntry  |       p     cBaseQuery  �       �  
   hQuery  �       �     cColumns    �       �     iTable  �       �  
   hBuffer          
   hColumn (             lResult H       <     cStripDisp           \     cStripEnable            X  |  RowObject      u  v  x  y  O  P  Q  R  i  u  v  w  y  {  |  }  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  F	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  N
  Z
  [
  ]
  ^
  _
  `
  f
  g
  h
  i
  j
  k
  l
  m
  n
  p
  q
  r
  s
  t
  u
  v
  w
  x
  y
  z
  {
  }
  ~
  �
  �
  �
  �
  �
                   	  
                                               !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  �  �  �  �  �  �  �  �  �  �      '  9  ^  z  |  �    3  4  8  B  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    &  F  P  �  �  �  �  	    2  O  c  z  �  |      N - C:\Progress\OpenEdge\src\adm2\brschnge.i p  � , C:\Progress\OpenEdge\src\adm2\brsscrol.i �  l� + C:\Progress\OpenEdge\src\adm2\brsleave.i �  0 * C:\Progress\OpenEdge\src\adm2\brsentry.i   �� ) C:\Progress\OpenEdge\src\adm2\brsoffhm.i @  �J ( C:\Progress\OpenEdge\src\adm2\brsoffnd.i t  ] ' C:\Progress\OpenEdge\src\adm2\brshome.i  �  Џ & C:\Progress\OpenEdge\src\adm2\brsend.i   �  �� % C:\Progress\OpenEdge\src\adm2\brsdefault.i     ��  C:\Progress\OpenEdge\src\adm2\browser.i  H  'z $ %C:\Progress\OpenEdge\src\adm2\custom\browsercustom.i |  }  C:\Progress\OpenEdge\src\adm2\datavis.i  �  � # %C:\Progress\OpenEdge\src\adm2\custom\dataviscustom.i �  ��  C:\Progress\OpenEdge\src\adm2\visual.i   0  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  d  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �  Ds ! C:\Progress\OpenEdge\gui\fn  �  tw   %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i      Q.  C:\Progress\OpenEdge\gui\set @  ��  C:\Progress\OpenEdge\src\adm2\brsprop.i  h  ��  %C:\Progress\OpenEdge\src\adm2\custom\brspropcustom.i �  !&  %C:\Progress\OpenEdge\src\adm2\custom\brsprtocustom.i �  ��  C:\Progress\OpenEdge\src\adm2\dvisprop.i    B�  %C:\Progress\OpenEdge\src\adm2\custom\dvispropcustom.i    P   ��  %C:\Progress\OpenEdge\src\adm2\custom\dvisprtocustom.i    �   F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �   �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i !  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i L!  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �!  �j  C:\Progress\OpenEdge\gui\get �!  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    �!  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    ,"  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i p"  Su  C:\Progress\OpenEdge\src\adm2\globals.i  �"  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i �"  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   #  �X  C:\Progress\OpenEdge\src\adm2\visprto.i  \#  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �#  �7 
 C:\Progress\OpenEdge\src\adm2\dvisprto.i �#  0 	 %C:\Progress\OpenEdge\src\adm2\custom\datavisdefscustom.i $  _�  C:\Progress\OpenEdge\src\adm2\brsprto.i  L$  t�  %C:\Progress\OpenEdge\src\adm2\custom\browserdefscustom.i �$  ��  C:\Progress\OpenEdge\src\adm2\robjflds.i �$  .X  D:\newsie\on_in_co\.\aplic\alm\dalmacen.i    �$  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   0%  ��    D:\newsie\on_in_co\aplic\alm\balmacen.w      b  �      �%       -   �%  X  u      �%  *   k  ,   �%     X  (   �%  &   T  ,   �%     J  )   �%  !   E  ,   &     &     &      %  ,   ,&          <&       ,   L&     �  (   \&     �  ,   l&     �     |&     �  ,   �&     �     �&     �  ,   �&  N  �      �&     }  +   �&  D  s      �&     f  *   �&  :  \      �&     R  )   '  0  H      '     5  (   ,'  &  +      <'        '   L'          \'       &   l'          |'     �  %   �'  �   �      �'  �   �     �'     o  $   �'  �   j     �'     H     �'  �   @     �'          �'  �        (     �     (  �   �     ,(     �     <(  a   v     L(  o   A     \(     �  #   l(  W   �     |(  n   �     �(     a  "   �(  i   \     �(     :     �(  N        �(  �   �     �(     �  !   �(  �   w     �(           )  �        )     �     ,)  �   �     <)     �     L)  �   �     \)     �     l)  �   �     |)     �     �)  �   x     �)     V     �)  �   S     �)     1     �)  }   %     �)          �)     �     �)     :     *     �     *  (   �     ,*  �   �     <*  O   �     L*     �     \*     5     l*  �   �
     |*  �   �
     �*  O   �
     �*     �
     �*     �
     �*  }   W
     �*  �   N
     �*  O   @
     �*     /
     �*     �	     +  �   �	     +  �  �	     ,+     y	     <+  �  F	     L+  O   8	     \+     '	     l+     �     |+  �        �+     �     �+     *     �+  x   $     �+          �+     �     �+     �     �+     |     �+     c     ,  f   ;     ,     �     ,,  "   �     <,     �     L,     a     \,  X   <     l,     �  
   |,      N     �,     :  	   �,          �,  b   �     �,     (     �,     �     �,     �     �,     �     �,     �     -  `   4      -          ,-  _         <-     �      L-  '   �       \-     '      