	��V�W}aX6  ��              �                                �� 36580112utf-8 MAIN d:\newsie\on_in_co\APLIC\adm\perfiles-erp-progress.w,, PROCEDURE tvNodeSelect,,INPUT pcnodeKey CHARACTER PROCEDURE tvnodeEvent,,INPUT pcEvent CHARACTER,INPUT pcnodeKey CHARACTER PROCEDURE tvNodeDropEnd,,INPUT pcEvent CHARACTER,INPUT pcnodeKey CHARACTER PROCEDURE tvNodeCreatePopup,,INPUT pcnodeKey CHARACTER PROCEDURE tvNodeAddOnExpand,,INPUT pcnodeKey CHARACTER PROCEDURE initializeObject,, PROCEDURE exitObject,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE cargar-treeview,,INPUT pParent CHARACTER PROCEDURE carga-usuarios,, PROCEDURE carga-grupos,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        �3              �             |U �3  0�              ��              �6    +   �� �  7   T� `  8   �� �   F   �� $G  G   �� �  H   ��    I   �� |  J   H� 4  K   |� $  L   �� l  M   � �
  N   �	 8  O   � �
  P   � l	  Q   @! �  R           �$ T  $' `  �) �  x, 8  ? �/ �%  iSO8859-1                                                                           �1   ( �                                      �                  0�                �2       4   �N   <�  (3    |3  ��  �   �3      �3          �                                             PROGRESS                         T           
    
                    �              �                                                                                                     
           �          \  8%  �   0&     �  �ɺ[�&  
                     (!          "      �   l         �       �   \  �*  �   �+  �   �  �ɺ[8,  
                     �&          �'      �   �         �       �   \  p0  �   h1  �   �  �ɺ[�1  
                     `,          <-      �   �         �       .  B  8%  �   0&     �  �ɺ[�&  
       .  �         (!          "      �                             �
      |  
    
                  h  0             �                                                                                          �
          
  �  �
      (  
    
                    �             �                                                                                          �
          
  X  �
      �  
    
                  �  �  	           D                                                                                          �
          
    �
      �  
    
                  l  4  
           �                                                                                          �
          
  �  �
      ,  
    
                    �             �                                                                                          �
          
  \        �  
    
                  �  �             H                                                                                                    
          �  
    
                  p  8             �                                                                                                    
  �  -      0  
    
                    �             �                                                                                          -          
  `	  ;      �                         �  �	             L	                                                                                          ;            
  H      �	                        t	  <
             �	                                                                                          H            �
  V      4
  
    
                   
  �
             �
                                                                                          V          
  d  d      �
  
    
                  �
  �             P                                                                                          d          
    r      �  
    
                  x  @             �                                                                                          r          
  �  �      8                        $  �             �                                                                                          �            h  �      �                        �  �             T                                                                                          �              �      �                        |  D                                                                                                        �                �      <                        (               �                                                                                          �                         INTEGRAL                         PROGRESS                               �  �      �                         �ɺ[            �  15                              �  �                      �  �  #      APLIC-IDUSER-IDCODCIAADMINSEGURIDAD                                                   �     �  �      �                         �ɺ[            �  ]                              �  �                      �  �  )      PROCEDIMIENTODETALLEGRUPOSAPLIC-IDVERSION                                                     �     n   �      n                          |��^            v   vh                              �  x                      �  �  s      CODMNUETIQUETATIPOPROGRAMASEGURIDAD-GRUPOSACCESO-DIRECTOICONAPLIC-IDSEGURIDAD-ATRIBUTOSPERSISTENTETECLA-ACELERADORA                                                                       	          
                                      �   �      �                          ��            �   t                              �                                        ��                                              � ��            �  �8h            
                                        
             
             
                                         
                                                                                                               
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
                                                                     �  �  �  �  �  �  �  �      (  8  H  X  h  x  \ � �  �      (  ,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������		,	<	L	\	l	|	�	�	�	�	�	�	�	�	

,
<
L
\
l
|
�
�
�
�
�
�
�
�
,<L\l|   �  �  �  �  �  �  �  �      (  8  H  X  h  x   \ ��  �      (  ,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������			,	<	L	\	l	|	�	�	�	�	�	�	�	�


,
<
L
\
l
|
�
�
�
�
�
�
�
�,<L\l|                                                                                                                               	                  
                                                  �#  �#  �#  $  $                         $   $  ($  0$                             4$  <$  H$  P$                              T$  \$  p$  x$                             |$  �$  �$  �$                              �$  �$  �$  �$                              �$  �$  �$  �$                             �$  �$  �$  �$                             �$  %  %  %                              %  $%  ,%  4%                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     %        %        $%                �     i  i      i  i      i  i     	 	 		 	    0   `   8   @   H   P   X   h   p   x                                                                                                                                  	                  
                                                  �)  �)  �)  �)  �)                         �)  �)  �)  �)                             �)  �)  �)  �)                              �)  �)  *  *                             *   *  0*  8*                              <*  D*  L*  T*                              X*  `*  l*  t*                             x*  �*  �*  �*                             �*  �*  �*  �*                              �*  �*  �*  �*                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     %        %        $%                �     i  i      i  i      i  i     	 	 		 	    0   `   8   @   H   P   X   h   p   x                                                                                                                                  	                  
                                                   /  (/  0/  L/  @/                         P/  X/  `/  h/                             l/  t/  �/  �/                              �/  �/  �/  �/                             �/  �/  �/  �/                              �/  �/  �/  �/                              �/  �/  0  0                             0  0  (0  00                             40  <0  H0  P0                              T0  \0  d0  l0                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     %        %        $%                �     i  i      i  i      i  i     	 	 		 	    0   `   8   @   H   P   X   h   p   x     ��                                                                                                                                            �          ����                            s    p�  2                 S�    �   l�  2                 S�    �   h�  2                 S�    %         �%   �4    �%   �8    �%   �    %         �%          %         getOtherWinTargetFrame  undefined                                                               �       t�  �   l   ��    ��                  �����               ��f                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D            C          assignFocusedWidget         �      �     �       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4           LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �           LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  -      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  B      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 [      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    f      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    v      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             d   �           �   �          �   �          �   �              � ߱            Z   �����
   �p
                         u   ���� �             �   �             �          <  �              � ߱            Z   ����$   �                         u   ���� �             `  �           �  �          �  �              � ߱            Z   �����   ��                     P    �  4  D  �  �      4   �����      o   �       x                              �  �  NA    �    �  (     <     P    d    x    �    �    �  `  �  
`  �  $  �               $  �  $  ���                       ,     
                    � ߱        ��    �  l  �      4      4   ����4                �                      ��                  �  �                  $�f                       �  |  |    �    $      h      4   ����h      $  �  P  ���                       �  @         �              � ߱              �  �  �             4   ����       $  �  �  ���                       P  @         <              � ߱        assignPageProperty                              �  �      ��                  y  |  �              ��f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  ~                   1g                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  �  �                 4Zf                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            constructObject                               �      ��                  �  �  ,              X�f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   x             D               �� 
  �             l  
             ��   �             �               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  �  �  �              ��f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  �  �  �              �f                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  �  �  �              �rg                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  �  �  �              �sg                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            initializeObject                                  �      ��                  �  �  ,              ��g                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               $        ��                  �  �  <              4�g                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               $        ��                  �  �  <              �g                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  T           ��                            ����                            notifyPage                              L  4      ��                  �  �  d              8bg                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            passThrough                             t  \      ��                  �  �  �              @�f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            removePageNTarget                               �  �      ��                  �  �  �              ��g                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0             �  
             ��                  $           ��                            ����                            selectPage                                        ��                  �  �  4               �g                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  L            ��                            ����                            toolbar                             @!  (!      ��                  �  �  X!              �~g                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p!           ��                            ����                            viewObject                              h"  P"      ��                  �  �  �"              tg                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                h#  P#      ��                  �  �  �#              �g                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �#           ��                            ����                            disablePagesInFolder    
       $      8$          LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder $      d$      �$          LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  x$      �$      �$    /      HANDLE, getCallerWindow �$       %      0%    B      HANDLE, getContainerMode    %      8%      l%    R      CHARACTER,  getContainerTarget  L%      x%      �%    c      CHARACTER,  getContainerTargetEvents    �%      �%      �%    v      CHARACTER,  getCurrentPage  �%       &      0&    �      INTEGER,    getDisabledAddModeTabs  &      <&      t&     �      CHARACTER,  getDynamicSDOProcedure  T&      �&      �&  !  �      CHARACTER,  getFilterSource �&      �&      �&  "  �      HANDLE, getMultiInstanceActivated   �&      �&      8'  #  �      LOGICAL,    getMultiInstanceSupported   '      D'      �'  $  �      LOGICAL,    getNavigationSource `'      �'      �'  %        CHARACTER,  getNavigationSourceEvents   �'      �'      (  &  $      CHARACTER,  getNavigationTarget �'      (      H(  '  >      HANDLE, getOutMessageTarget ((      P(      �(  (  R      HANDLE, getPageNTarget  d(      �(      �(  )  f      CHARACTER,  getPageSource   �(      �(      �(  *  u      HANDLE, getPrimarySdoTarget �(       )      4)  +  �      HANDLE, getReEnableDataLinks    )      <)      t)  ,  �      CHARACTER,  getRunDOOptions T)      �)      �)  -  �      CHARACTER,  getRunMultiple  �)      �)      �)  .  �      LOGICAL,    getSavedContainerMode   �)      �)      0*  /  �      CHARACTER,  getSdoForeignFields *      <*      p*  0  �      CHARACTER,  getTopOnly  P*      |*      �*  1 
 �      LOGICAL,    getUpdateSource �*      �*      �*  2         CHARACTER,  getUpdateTarget �*      �*       +  3        CHARACTER,  getWaitForObject     +      ,+      `+  4         HANDLE, getWindowTitleViewer    @+      h+      �+  5  1      HANDLE, getStatusArea   �+      �+      �+  6  F      LOGICAL,    pageNTargets    �+      �+      ,  7  T      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �+      L,      |,  8  a      LOGICAL,INPUT h HANDLE  setCallerProcedure  \,      �,      �,  9  q      LOGICAL,INPUT h HANDLE  setCallerWindow �,      �,      -  :  �      LOGICAL,INPUT h HANDLE  setContainerMode    �,      (-      \-  ;  �      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  <-      �-      �-  <  �      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �-      �-      .  =  �      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �-      (.      `.  >  �      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  @.      �.      �.  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �.      �.      /  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �.      8/      l/  A        LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   L/      �/      �/  B        LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �/      �/      40  C  2      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource 0      d0      �0  D  L      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   x0      �0      �0  E  `      LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �0      1      P1  F  z      LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget 01      p1      �1  G  �      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �1      �1      �1  H  �      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �1      2      H2  I  �      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget (2      h2      �2  J  �      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    |2      �2      �2  K  �      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �2      (3      X3  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions 83      x3      �3  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �3      �3      �3  N        LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �3       4      X4  O        LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields 84      �4      �4  P  -      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �4      �4      5  Q 
 A      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �4      05      `5  R  L      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget @5      �5      �5  S  \      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �5      �5      6  T  l      LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �5      ,6      d6  U  }      LOGICAL,INPUT phViewer HANDLE   getObjectType   D6      �6      �6  V  �      CHARACTER,  setStatusArea   �6      �6      �6  W  �      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �7  �7      ��                  ;  <  �7              �f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �8  �8      ��                  >  ?  �8              ��f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �9  �9      ��                  A  B  �9              Hf                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �:  �:      ��                  D  E  �:              �Hf                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �;  �;      ��                  G  I  �;              �f                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �;           ��                            ����                            getAllFieldHandles  �6      P<      �<  X  �      CHARACTER,  getAllFieldNames    d<      �<      �<  Y  �      CHARACTER,  getCol  �<      �<      �<  Z  �      DECIMAL,    getDefaultLayout    �<      =      8=  [  �      CHARACTER,  getDisableOnInit    =      D=      x=  \  �      LOGICAL,    getEnabledObjFlds   X=      �=      �=  ]  �      CHARACTER,  getEnabledObjHdls   �=      �=      �=  ^        CHARACTER,  getHeight   �=      >      0>  _ 	       DECIMAL,    getHideOnInit   >      <>      l>  `  )      LOGICAL,    getLayoutOptions    L>      x>      �>  a  7      CHARACTER,  getLayoutVariable   �>      �>      �>  b  H      CHARACTER,  getObjectEnabled    �>      �>      ,?  c  Z      LOGICAL,    getObjectLayout ?      8?      h?  d  k      CHARACTER,  getRow  H?      t?      �?  e  {      DECIMAL,    getWidth    |?      �?      �?  f  �      DECIMAL,    getResizeHorizontal �?      �?      @  g  �      LOGICAL,    getResizeVertical   �?       @      T@  h  �      LOGICAL,    setAllFieldHandles  4@      `@      �@  i  �      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    t@      �@      �@  j  �      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �@      A      <A  k  �      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    A      `A      �A  l  �      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   tA      �A      �A  m  �      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �A      B      8B  n  	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout B      \B      �B  o  	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal lB      �B      �B  p  &	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �B      C      DC  q  :	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated $C      lC      �C  r  L	      LOGICAL,    getObjectSecured    �C      �C      �C  s  `	      LOGICAL,    createUiEvents  �C      �C      D  t  q	      LOGICAL,    bindServer                              �D  �D      ��                  +  ,  �D              
f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �E  �E      ��                  .  /  �E              �f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �F  �F      ��                  1  2  �F              ��e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �G  �G      ��                  4  5  �G              \�e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �H  �H      ��                  7  8  �H              (�e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �I  �I      ��                  :  ;  �I              ��e                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �J  �J      ��                  =  ?  �J              |�e                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 K  
         ��                            ����                            startServerObject                               L  �K      ��                  A  B  ,L              �/f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                M   M      ��                  D  F  0M              p0f                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  HM           ��                            ����                            getAppService   �C      �M      �M  u  �	      CHARACTER,  getASBound  �M      �M      N  v 
 �	      LOGICAL,    getAsDivision   �M      $N      TN  w  �	      CHARACTER,  getASHandle 4N      `N      �N  x  �	      HANDLE, getASHasStarted lN      �N      �N  y  �	      LOGICAL,    getASInfo   �N      �N      �N  z 	 �	      CHARACTER,  getASInitializeOnRun    �N      O      @O  {  �	      LOGICAL,    getASUsePrompt   O      LO      |O  |  �	      LOGICAL,    getServerFileName   \O      �O      �O  }  �	      CHARACTER,  getServerOperatingMode  �O      �O       P  ~  
      CHARACTER,  runServerProcedure  �O      P      @P    
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService    P      �P      �P  �  -
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �P      �P      Q  �  ;
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �P      0Q      \Q  �  I
      LOGICAL,INPUT phASHandle HANDLE setASInfo   <Q      |Q      �Q  � 	 U
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �Q      �Q       R  �  _
      LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �Q      $R      TR  �  t
      LOGICAL,INPUT plFlag LOGICAL    setServerFileName   4R      tR      �R  �  �
      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �R      �R      S  �  �
      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �S  �S      ��                  	    �S              lBg                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $T             �S  
             ��   LT             T               �� 
                 @T  
         ��                            ����                            addMessage                              8U   U      ��                      PU              �{f                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �U             hU               ��   �U             �U               ��                  �U           ��                            ����                            adjustTabOrder                              �V  �V      ��                      �V              �Hg                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  W             �V  
             �� 
  @W             W  
             ��                  4W           ��                            ����                            applyEntry                              ,X  X      ��                      DX              �tg                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \X           ��                            ����                            changeCursor                                XY  @Y      ��                    !  pY              8�f                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �Y           ��                            ����                            createControls                              �Z  lZ      ��                  #  $  �Z              |�f                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �[  p[      ��                  &  '  �[              4\g                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �\  t\      ��                  )  *  �\              _g                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �]  �]      ��                  ,  -  �]              h                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �^  �^      ��                  /  0  �^              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �_  �_      ��                  2  3  �_              �	                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �`  �`      ��                  5  6  �`              ȷ
                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �a  �a      ��                  8  =  �a              �
                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  b             �a  
             ��   4b              b               ��   \b             (b               ��                  Pb           ��                            ����                            modifyUserLinks                             Lc  4c      ��                  ?  C  dc               �
                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �c             |c               ��   �c             �c               �� 
                 �c  
         ��                            ����                            removeAllLinks                              �d  �d      ��                  E  F  �d               A                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �e  �e      ��                  H  L  �e              �A                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ,f             �e  
             ��   Tf              f               �� 
                 Hf  
         ��                            ����                            repositionObject                                Hg  0g      ��                  N  Q  `g              ,�
                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             xg               ��                  �g           ��                            ����                            returnFocus                             �h  �h      ��                  S  U  �h              9                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �h  
         ��                            ����                            showMessageProcedure                                �i  �i      ��                  W  Z  �i              ,'
                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0j             �i               ��                  $j           ��                            ����                            toggleData                              k  k      ��                  \  ^  4k              (
                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  Lk           ��                            ����                            viewObject                              Dl  ,l      ��                  `  a  \l              �/                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �R      �l      �l  � 
 �      LOGICAL,    assignLinkProperty  �l      �l       m  �        LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages    m      xm      �m  �        CHARACTER,  getChildDataKey �m      �m      �m  �  &      CHARACTER,  getContainerHandle  �m      �m      $n  �  6      HANDLE, getContainerHidden  n      ,n      `n  �  I      LOGICAL,    getContainerSource  @n      ln      �n  �  \      HANDLE, getContainerSourceEvents    �n      �n      �n  �  o      CHARACTER,  getContainerType    �n      �n      $o  �  �      CHARACTER,  getDataLinksEnabled o      0o      do  �  �      LOGICAL,    getDataSource   Do      po      �o  �  �      HANDLE, getDataSourceEvents �o      �o      �o  �  �      CHARACTER,  getDataSourceNames  �o      �o      p  �  �      CHARACTER,  getDataTarget   �o      (p      Xp  �  �      CHARACTER,  getDataTargetEvents 8p      dp      �p  �  �      CHARACTER,  getDBAware  xp      �p      �p  � 
       LOGICAL,    getDesignDataObject �p      �p      q  �        CHARACTER,  getDynamicObject    �p      q      Pq  �  #      LOGICAL,    getInstanceProperties   0q      \q      �q  �  4      CHARACTER,  getLogicalObjectName    tq      �q      �q  �  J      CHARACTER,  getLogicalVersion   �q      �q      r  �  _      CHARACTER,  getObjectHidden �q      $r      Tr  �  q      LOGICAL,    getObjectInitialized    4r      `r      �r  �  �      LOGICAL,    getObjectName   xr      �r      �r  �  �      CHARACTER,  getObjectPage   �r      �r      s  �  �      INTEGER,    getObjectParent �r      s      Ls  �  �      HANDLE, getObjectVersion    ,s      Ts      �s  �  �      CHARACTER,  getObjectVersionNumber  hs      �s      �s  �  �      CHARACTER,  getParentDataKey    �s      �s      t  �  �      CHARACTER,  getPassThroughLinks �s      t      Lt  �  �      CHARACTER,  getPhysicalObjectName   ,t      Xt      �t  �        CHARACTER,  getPhysicalVersion  pt      �t      �t  �  %      CHARACTER,  getPropertyDialog   �t      �t      u  �  8      CHARACTER,  getQueryObject  �t      u      Lu  �  J      LOGICAL,    getRunAttribute ,u      Xu      �u  �  Y      CHARACTER,  getSupportedLinks   hu      �u      �u  �  i      CHARACTER,  getTranslatableProperties   �u      �u      v  �  {      CHARACTER,  getUIBMode  �u      v      Hv  � 
 �      CHARACTER,  getUserProperty (v      Tv      �v  �  �      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    dv      �v      �v  �  �      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �v      w      8w  �  �      CHARACTER,INPUT pcLink CHARACTER    linkProperty    w      \w      �w  �  �      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry lw      �w      �w  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �w      `x      �x  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    px      �x      �x  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �x      y      <y  �        CHARACTER,  setChildDataKey y      Hy      xy  �        LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  Xy      �y      �y  �  $      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �y      �y      (z  �  7      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    z      Hz      �z  �  J      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled dz      �z      �z  �  c      LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �z      {      4{  �  w      LOGICAL,INPUT phObject HANDLE   setDataSourceEvents {      T{      �{  �  �      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  h{      �{      �{  �  �      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �{      |      <|  �  �      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents |      `|      �|  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  t|      �|      �|  � 
 �      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �|      }      8}  �  �      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    }      `}      �}  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   t}      �}      �}  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �}      ~      D~  �        LOGICAL,INPUT c CHARACTER   setLogicalVersion   $~      `~      �~  �  )      LOGICAL,INPUT cVersion CHARACTER    setObjectName   t~      �~      �~  �  ;      LOGICAL,INPUT pcName CHARACTER  setObjectParent �~            8  �  I      LOGICAL,INPUT phParent HANDLE   setObjectVersion          X      �  �  Y      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    l      �      �  �  j      LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �      �      D�  �  {      LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   $�      d�      ��  �  �      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  |�      ��      ��  �  �      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute Ѐ      �      D�  �  �      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   $�      l�      ��  �  �      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ā       �  �  �      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      $�      P�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty 0�      p�      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      �  �        LOGICAL,INPUT pcMessage CHARACTER   Signature   �      0�      \�  � 	       CHARACTER,INPUT pcName CHARACTER    T�    w  ��  �      �      4   �����                (�                      ��                  x  �                  �
                       x  ��        y  D�  ��      �      4   �����                Є                      ��                  z  �                  �4
                       z  T�  Ѕ    �  �  h�      �      4   �����                x�                      ��                  �  �                  P5
                       �  ��         �                                  x     
                    � ߱        ��  $  �  ��  ���                           $  �  (�  ���                       �                         � ߱        `�    �  p�  �      �      4   �����                ��                      ��                  �  n	                  6
                       �  ��  0�  o   �      ,                                 ��  $   �  \�  ���                       H  @         4              � ߱        ��  �   �  h      ��  �   �  �      ć  �   �  P      ؇  �   �  �      �  �   �  8       �  �   �  �      �  �   �  (	      (�  �   �  d	      <�  �   �  �	      P�  �   �  L
      d�  �   �  �
      x�  �   �  D      ��  �   �  �      ��  �   �  �      ��  �   �  x      Ȉ  �   �  �      ܈  �   �  (      ��  �   �  �      �  �   �  �      �  �   �  L      ,�  �   �  �      @�  �   �  <      T�  �   �  �      h�  �   �  ,      |�  �   �  �      ��  �   �        ��  �   �  �      ��  �   �  �      ̉  �   �  @      ��  �   �  |      �  �   �  �      �  �   �  ,      �  �   �  h      0�  �   �  �      D�  �   �  �      X�  �   �  \      l�  �   �  �      ��  �   �  �      ��  �   �        ��  �   �  L      ��  �   �  �      Њ  �   �  �      �  �   �         ��  �   �  <          �   �  x                      $�          ��  x�      ��                  �	  �	  ��              <~
                    O   ����    e�          O   ����    R�          O   ����    ��      �     
                d       	       	       t                         � ߱        P�  $ �	  ��  ���                           O   �	  ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                �6      �      h�     6     Č                      V ��  �                      �    �	  |�  ��      �      4   �����                �                      ��                  �	  j
                  �
                       �	  ��  �  �   �	         0�  �   �	  �      D�  �   �	        X�  �   �	  �      l�  �   �	        ��  �   �	  �      ��  �   �	  �      ��  �   �	  t      ��  �   �	  �      Ў  �   �	  l      �  �   �	  �      ��  �   �	  \      �  �   �	  �          �   �	  T      ��    u
  <�  ��      �      4   �����                ȏ                      ��                  v
                    l�
                       v
  L�  ܏  �   x
  $       ��  �   y
  �       �  �   z
  !      �  �   {
  �!      ,�  �   |
  �!      @�  �   }
  p"      T�  �   ~
  �"      h�  �   
  `#      |�  �   �
  �#      ��  �   �
  H$      ��  �   �
  �$      ��  �   �
  8%      ̐  �   �
  �%      ��  �   �
  (&      ��  �   �
  �&      �  �   �
   '      �  �   �
  �'      0�  �   �
  (      D�  �   �
  �(      X�  �   �
  )      l�  �   �
  �)      ��  �   �
  *      ��  �   �
  �*      ��  �   �
   +      ��  �   �
  |+      Б  �   �
  �+      �  �   �
  t,          �   �
  �,      �      �  ��      X-      4   ����X-                ��                      ��                    �                  ��
                         $�  ��  �     �-      Ȓ  �     4.      ܒ  �     �.      �  �     $/      �  �     �/      �  �     0      ,�  �     �0      @�  �     �0      T�  �     01      h�  �     l1      |�  �      �1      ��  �   !  2      ��  �   "  �2      ��  �   #  3      ̓  �   %  �3      ��  �   &  �3      ��  �   '  h4      �  �   (  �4      �  �   )  `5      0�  �   *  �5      D�  �   ,  6      X�  �   -  �6      l�  �   .  �6      ��  �   /  47      ��  �   0  p7      ��  �   1  �7      ��  �   2  (8      Д  �   3  d8      �  �   4  �8      ��  �   5  �8      �  �   6  9       �  �   7  T9      4�  �   8  �9      H�  �   :  :      \�  �   ;  @:      p�  �   <  |:      ��  �   =  �:      ��  �   >  �:      ��  �   ?  0;      ��  �   @  l;      ԕ  �   A  �;      �  �   B  <      ��  �   C  �<      �  �   D  =      $�  �   E  x=      8�  �   F  �=      L�  �   G  p>      `�  �   H  �>      t�  �   I  h?      ��  �   J  �?      ��  �   K  `@      ��  �   L  �@      Ė  �   M  A      ؖ  �   N  TA      �  �   O  �A       �  �   P  �A          �   Q  @B      l�  $  �  @�  ���                       �B     
  
       
           � ߱        �      ��  ��      �B      4   �����B      /     ė     ԗ                          3   �����B            ��                      3   �����B  X�       �  ��  ��   C      4   ���� C  	              ��                      ��             	       �                  �C
                         0�  ��  �     `C      �  $    �  ���                       �C     
                    � ߱        ,�  �     �C      ��  $     X�  ���                       �C  @         �C              � ߱        @�  $    ��  ���                       (D                         � ߱        �D     
                E       	       	       hF  @        
 (F              � ߱        К  V   '  ܙ  ���                        tF                     �F                     �F                         � ߱        `�  $  C  l�  ���                       �G     
                 H       	       	       pI  @        
 0I              � ߱        �  V   U  ��  ���                        |I     
                �I       	       	       HK  @        
 K              � ߱            V   z  ��  ���                        
              P�                      ��             
     �  5                  ��	                       �  �  TK     
                �K       	       	        M  @        
 �L          �M  @        
 DM          �M  @        
 �M          DN  @        
 N              � ߱            V   �  ��  ���                        adm-clone-props �  |�              �     7     `                          \  e                     start-super-proc    ��  �  �           �     8                                  �                     �    M  t�  ��      �Q      4   �����Q      /   N  ��     ��                          3   �����Q            ��                      3   ���� R  H�  $  h  �  ���                        R                         � ߱        �    x  d�  ��  ��  <R      4   ����<R                T�                      ��                  y  }                  ��                       y  t�  PR                     dR                     xR                         � ߱            $  z  �  ���                             ~  ��  ؠ      �R      4   �����R  �R                         � ߱            $    ��  ���                        �    �   �  0�  ��  �R      4   �����R      $  �  \�  ���                       �R                         � ߱            �   �  �R      8S     
                �S       	       	       U  @        
 �T              � ߱        ,�  V   �  ��  ���                        @�  �   �  U      آ    m  \�  l�      PU      4   ����PU      /   n  ��     ��                          3   ����`U            Ȣ                      3   �����U  ��  $  r  �  ���                       �U                         � ߱        �U     
                DV       	       	       �W  @        
 TW              � ߱        ��  V   |  0�  ���                        ��    �  ܣ  X�      �W      4   �����W                h�                      ��                  �  �                  ��
                       �  �      g   �  ��         ��D�                           H�          �   �      ��                  �      0�              4�
                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  t�     ��  �W                      3   �����W  ��     
   ��                      3   �����W         
   ԥ                      3   �����W    ��                              ��        �                  ����                                        ��              9      �                      g                               ��  g   �  ��          ��	L�                           ��          P�  8�      ��                  �  �  h�              ��	                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  ��     ��   X                      3   �����W            ܧ                      3   ����X    ��                              ��        �                  ����                                        ̦              :      �                      g                               ��  g     ��          ��	T�                           ��          X�  @�      ��                      p�              T�	                    O   ����    e�          O   ����    R�          O   ����    ��          /    ��     ĩ  @X                      3   ����$X            �                      3   ����HX    ��                              ��        �                  ����                                        Ԩ              ;      ��                      g                               �      ̪  H�      dX      4   ����dX                X�                      ��                    :                  �	                         ܪ  ī  /     ��     ��                          3   ����tX            ��                      3   �����X  ��  /    �      �  �X                      3   �����X  0�     
    �                      3   �����X  `�        P�                      3   �����X  ��        ��                      3   �����X            ��                      3   ����Y  �    &  ܬ  �      <Y      4   ����<Y      /  ,  �     (�  �Y                      3   �����Y  X�     
   H�                      3   �����Y  ��        x�                      3   �����Y  ��        ��                      3   �����Y            ح                      3   ����Z        2  �  �      ,Z      4   ����,Z      /  5  @�     P�  �Z                      3   ����`Z  ��     
   p�                      3   �����Z  ��        ��                      3   �����Z  �        Ю                      3   �����Z             �                      3   �����Z  б    >  ,�  ��      �Z      4   �����Z                ��                      ��                  ?  B                  �#
                       ?  <�      g   @  Я         ��t�        �Z                  ��          h�  P�      ��                  A      ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��          /  A  İ     ԰  [                      3   ���� [  �     
   ��                      3   ����$[         
   $�                      3   ����,[    ��                            ����                                        �              <      4�                      g                               h�     F  4[                                     H[     
                �[       	       	       ]  @        
 �\              � ߱        ��  V   �  �  ���                        (]     
                �]       	       	       �^  @        
 �^              � ߱        $�  V   �  ��  ���                        ��      @�  P�      _      4   ����_      $     |�  ���                       h_  @         T_              � ߱        |�  g   N  ��         �� �        |_  �� �        �_                  ��          l�  T�      ��                  O  T  ��              |�
                    O   ����    e�          O   ����    R�          O   ����    ��            S  ��  ȴ      �_      4   �����_      O  S  ������  �_    ��                            ����                                        �              =      �                      g                               (�  g   [  ��         �6̶         �_                  \�          ,�  �      ��                  \  a  D�              ܏
                    O   ����    e�          O   ����    R�          O   ����    ��      t�    _  �_  }          O  `  ������  �_    ��                            ����                                        ��              >      ��                      g                               (�  g   j  @�         �!̸                            �          ط  ��      ��                  k  p  �              `�
                    O   ����    e�          O   ����    R�          O   ����    ��      �  �   l  �_      X�  /   m  H�                                 3   ����`      �   n  ,`        ��                              ��        �                  ����                                        T�              ?      l�                      g                               <�  g   w  @�         �4�                            �          ع  ��      ��                  x  ~  �              (�	                    O   ����    e�          O   ����    R�          O   ����    ��      �  �   y  L`      0�  �   z  l`      l�  /   {  \�                                 3   �����`      �   |  �`        ��                              ��        �                  ����                                        T�              @      ��                      g                               ��  g   �  T�         �4d�                           H�          �  Ի      ��                  �  �  �              ��	                    O   ����    e�          O   ����    R�          O   ����    ��                                � ߱        t�  $   �  �   �                       ̼  $  �  ��  ���                       �`                         � ߱            s   �  ��       Ⱦ                      $�  t�  D�                               7   ����          ����                a   �            Ľ                  6   �         �  ����               a   �            Ľ                                                                0�  $�                                   @            �   �        J   �        ����    ��                                                          La  pa  �a                      ��                 �`   �`   �`    ��                              ��        �                  ����                            s        2                 S�                h�              A      �             @�      g                               ��  g   �  ؿ         �4<�                           ��          p�  X�      ��                  �  �  ��              �/
                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  ��                                 3   �����a    ��                              ��        �                  ����                                        �              B      ��                      g                               ��    �  ��  0�      �a      4   �����a                ��                      ��                  �  �                  �
                       �  ��  �a  @                     b  @         �a          8b  @         $b              � ߱        ��  $   �  @�  ���                       ��  g   �  ��         �np�      }                      ��          ��  h�      ��                  �  �  ��              �
                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /  �  ��                                 3   ����Db        �  �  �      `b      4   ����`b      O  �  ������  �b    ��                            ����                                        ��              C      0�                      g                               ��  g   �  ��         �!D�         �b                  ��          |�  d�      ��                  �  �  ��              $
                    O   ����    e�          O   ����    R�          O   ����    ��      �b  @                         � ߱            $  �  ��  ���                         ��                            ����                                        ��              D      �                      g                               ��  /   �  ��                                 3   �����b        �  ��  t�      �b      4   �����b                ��                      ��                  �  �                  �
                       �  �                0�          �   �      ��                 �  �                  �*
                       �  ��      O   �    ��          O   �    ��      l�  /   �  \�                                 3   �����b        �  ��  ��      c      4   ����c      k   �  ��              }       n        �   ��  g   �  ��         �}��                           ��          |�  d�      ��                 �  �  ��              �E                    O   ����    e�          O   ����    R�          O   ����    ��      \�    �  ��  ��      (c      4   ����(c      O   �  ��  ��  `c    l�      ��  ,�                      ��        0         �  �                  �	      �c            �  ��      $  �  ��  ���                       tc                         � ߱        �  $  �  ��  ���                       �c                         � ߱            4   �����c  ��  $  �  X�  ���                       �c     
                    � ߱        ��  $   �  ��  ���                       4d  @          d              � ߱        4�  $   �  �  ���                       `d  @         Ld              � ߱            $   �  `�  ���                       �d  @         �d              � ߱          ��                              ��        �                  ����                                        ��              E      ��                      g                                 ��      �  ��                      ��        0         �                     ��	      (e     ��     �  H�      $  �  ��  ���                       �d                         � ߱        t�  $  �  H�  ���                       �d                         � ߱            4   ���� e  ��  $  �  ��  ���                       <e     
     \e             � ߱        4�  $  �  �  ���                       he     
                    � ߱              �  P�  `�      �e      4   �����e      $   �  ��  ���                       �e  @         �e              � ߱            $    ��  ���                       �e                         � ߱        adm-create-objects  ��  �                      F      �                               �                     carga-grupos    $�  ��          �+   ,    G     �E             �F          �E  �                      carga-usuarios  ��  ��                      H      ,             �              �                      cargar-treeview ��  X�  �           �    ! I     �                          �  !                     disable_UI  h�  ��                      J      <                              %!  
                   enable_UI   ��  ,�                      K      h             �              0!  	                   exitObject  8�  ��                      L      �                               :!  
                   initializeObject    ��  ��                      M      ,                              E!                     tvNodeAddOnExpand   �  l�  �       	  D	  # " N    <
  �                      8
  �!                     tvNodeCreatePopup   ��  ��  �           �    $ O     �                          �  �"                     tvNodeDropEnd   ��  L�  �           �	    % P     �
                          �
  N#                     tvnodeEvent \�  ��  �                & Q     	                          	  �$                     tvNodeSelect    ��   �  �           ,    ' R     P                          L  	%                      �<Todos>���   �   � ���  �       � ��    / ? O _ o  ���������������������������������������������������������������������������������������������������������������������������������������������������������            ��  8   ����   ��  8   ����   ��  8   ����    ��  8   ����    ��  8   ����   ��  8   ����   ��  8   ����   �  8   ����         �  8   ����   ,�  8   ����   <�  8   ����   L�  8   ����   \�  8   ����   l�  8   ����       8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    |�  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��   �  ,�      returnFocus ,INPUT hTarget HANDLE   �  T�  h�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    D�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  �      removeAllLinks  ,   ��  (�  8�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE �  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  �  (�      hideObject  ,   �  <�  T�      editInstanceProperties  ,   ,�  h�  x�      displayLinks    ,   X�  ��  ��      createControls  ,   |�  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��  ��  ��      applyEntry  ,INPUT pcField CHARACTER    ��  $�  4�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER �  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER |�  ��  ��      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  L�  \�      unbindServer    ,INPUT pcMode CHARACTER <�  ��  ��      startServerObject   ,   t�  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  ��      restartServerObject ,   ��  �  (�      initializeServerObject  ,    �  <�  P�      disconnectObject    ,   ,�  d�  x�      destroyServerObject ,   T�  ��  ��      bindServer  ,   |�  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ��  ��      enableObject    ,   ��  �  �      disableObject   ,   ��  0�  <�      applyLayout ,    �  P�  \�      viewPage    ,INPUT piPageNum INTEGER    @�  ��  ��      viewObject  ,   x�  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  �  (�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  d�  p�      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  T�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��  ��  ��      initPages   ,INPUT pcPageList CHARACTER ��  $�  @�      initializeVisualContainer   ,   �  T�  `�      hidePage    ,INPUT piPageNum INTEGER    D�  ��  ��      destroyObject   ,   |�  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  ��      createObjects   ,   ��  �  �      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE ��  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      changePage  ,   ��  ��   �      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 f%     adecomm/as-utils.w 
"   
   �    }        �
"     
   4         %              4       %              4       %              4       %              4         %              4       %              4       %              4         %              4       %              4       %                  �     }        �G� �   �G%              � �     %        %       %       	 %       	%       	 %       	%               %               %               %              %              %              %               %              
�        
"   
 f
�    
"   
 f
"   
 �    �        P     �        \    
"   
   �        �         �     }        �%              
"   
 f
"   
 �    �        �     �        �    
"   
   �        0         �     }        �%              � 
"    
 �%              � �  �         �      T     @     $              
�    � %   �     
"   
 � %   �     
�             �G                      
�            � '   
"    
 �
�H T   %              �     }        �GG %              � 
"   
   P �L 
�H T   %              �     }        �GG %              
"   
   �        (    7%               
"   
 ��           \    1� 7  
 �� B   �%               o%   o           � G    �
"   
 ��           �    1� H   �� B   �%               o%   o           � V   �
"   
 ��           D    1� ]  
 �� B   �%               o%   o           � h   �
"   
 ��           �    1� t   �� B   �%               o%   o           � �   �
"   
 ��           ,    1� �   �� B   �%               o%   o           � �   �
"   
 ��           �    1� �   �� �   �%               o%   o           %               
"   
 ��          	    1� �   �� �     
"   
 ��           X	    1� �   �� B   �%               o%   o           � �  e �
"   
 ��           �	    1� S   �� B   �%               o%   o           � b  [ �
"   
 ��           @
    1� �   �� �   �%               o%   o           %               
"   
 ��           �
    1� �   �� �   �%               o%   o           %               
"   
 ��           8    1� �   �� �   �%               o%   o           %              
"   
 ��          �    1� �   �� �     
"   
 ��           �    1� �  
 �� �   �%               o%   o           %               
"   
 ��           l    1�    �� B   �%               o%   o           � G    �
"   
 ��          �    1�    �� �     
"   
 ��               1�    �� B   �%               o%   o           � 5  t �
"   
 ��          �    1� �  
 �� �     
"   
 ��           �    1� �   �� B   �%               o%   o           � �  � �
"   
 ��           @    1� S   �� B   �%               o%   o           � G    �
"   
 ��           �    1� j  
 �� u   �%               o%   o           %               
"   
 
�           0    1� y   
� �   �%               o%   o           %               
"   
 �           �    1� �   � B   �%               o%   o           � G    

"   
 �                1� �   � B   �%               o%   o           o%   o           
"   
 	�           �    1� �  
 	� B   �%               o%   o           � G    
"   
 �               1� �   � �  	 �%               o%   o           � �  / 	
"   
 ��          �    1� �   �� �  	   
"   
 �           �    1� 
   � �  	 �o%   o           o%   o           � G    
"   
 ��          4    1�    �� �  	   
"   
 f�           p    1� ,   f� �  	 �o%   o           o%   o           � G    f
"   
 ��          �    1� <   �� �     
"   
 ��               1� J   �� �  	   
"   
 ��          \    1� W   �� �  	   
"   
 ��          �    1� d   �� �  	   
"   
 �           �    1� r   � �   �o%   o           o%   o           %              
"   
 ��          P    1� �   �� �  	   
"   
 ��          �    1� �  
 �� �     
"   
 ��          �    1� �   �� �  	   
"   
 ��              1� �   �� �  	   
"   
 ��          @    1� �   �� �  	   
"   
 ��          |    1� �   �� �  	   
"   
 ��          �    1� �  	 �� �  	   
"   
 ��          �    1� �   �� �  	   
"   
 ��          0    1�    �� �  	   
"   
 �           l    1�    � B   �%               o%   o           o%   o           
�H T   %              �     }        �GG %              
"   
   
"   
 

"   
   
"   
 �(�  L ( l       �        4    �� *   � P   �        @    �@    
� @  , 
�       L    �� 3     p�               �L
�    %              � 8      X    � $         � :          
�    � T     
"   
 �� @  , 
�       h    �� ]  
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
"   
 
�               1� W  
 
� B   �%               o%   o           � G    

"   
 
�           �    1� b  
 
� B   �%               o%   o           o%   o           
"   
 �               1� m   � �   �%               o%   o           o%   o           
"   
 �           �    1� v   � �   �%               o%   o           %               
"   
 
�           �    1� �   
� �   �%               o%   o           %               
"   
 f�           x    1� �   f� B   �%               o%   o           � G    

"   
 �           �    1� �   � �   �%               o%   o           %              
"   
 �           h    1� �   � �   �%               o%   o           o%   o           
"   
 	�           �    1� �   	� B   �%               o%   o           o%   o           
"   
 �           `    1� �  	 � B   �%               o%   o           � G    
"   
 �           �    1� �   � B   �%               o%   o           o%   o           
"   
 
�           P    1� �   
� B   �%               o%   o           o%   o           
"   
 
�           �    1� �   
� �   �%               o%   o           %               
"   
 
�           H    1�    
� �   �%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
"   
 
�                1�    
� �  	 �%               o%   o           � G    

"   
 	�           �     1�    	� �  	 �%               o%   o           � G    

"   
 
�            !    1� )   
� �   �%               o%   o           %               
"   
 f�           |!    1� 7   f� �  	 �%               o%   o           � G    

"   
 
�           �!    1� F   
� �  	 �%               o%   o           � G    f
"   
 �           d"    1� T   � �   �%               o%   o           %               
"   
 �           �"    1� b   � �  	 �%               o%   o           � G    
"   
 �           T#    1� q   � �  	 �%               o%   o           � G    
"   
 
�           �#    1� �   
� �  	 �%               o%   o           � G    
"   
 
�           <$    1� �   
� �  	 �%               o%   o           o%   o           
"   
 
�           �$    1� �   
� �  	 �%               o%   o           � G    	
"   
 f�           ,%    1� �   f� �  	 �%               o%   o           � G    

"   
 
�           �%    1� �  	 
� �   �%               o%   o           %               
"   
 �           &    1� �   � �   �%               o%   o           %               
"   
 �           �&    1� �   � �   �%               o%   o           o%   o           
"   
 �           '    1� �   � �   �%               o%   o           o%   o           
"   
 
�           �'    1� �   
� �   �%               o%   o           %               
"   
 	�           (    1� �   	� �   �%               o%   o           %               
"   
 
�           �(    1�    
� �   �%               o%   o           %               
"   
 f�           )    1� !   f� -   �%               o%   o           %       
       
"   
 f�           �)    1� 5   f� -   �%               o%   o           o%   o           
"   
 
�           �)    1� A   
� -   �%               o%   o           %              
"   
 
�           x*    1� M   
� -   �%               o%   o           o%   o           
"   
 �           �*    1� Y   � -   �%               o%   o           %              
"   
 �           p+    1� f   � -   �%               o%   o           o%   o           
"   
 	�           �+    1� s   	� -   �%               o%   o           %              
"   
 	�           h,    1� {   	� -   �%               o%   o           o%   o           
"   
 f�           �,    1� �   f� �  	 �%               o%   o           � G    P �L 
�H T   %              �     }        �GG %              
"   
 
�           �-    1� �   
� u   �%               o%   o           %               
"   
 
�           (.    1� �   
� u   �%               o%   o           o%   o           
"   
 �           �.    1� �   � B   �%               o%   o           � G    

"   
 �           /    1� �   � B   �%               o%   o           � �  - 
"   
 
�           �/    1�    
� B   �%               o%   o           � G    
"   
 	�            0    1�    	� B   �%               o%   o           � 5   

"   
 ��          t0    1� S   �� �     
"   
 �           �0    1� d   � B   �%               o%   o           � G    

"   
 ��          $1    1� p  
 �� �     
"   
 ��          `1    1� {   �� �     
"   
 �           �1    1� �   � �  	 �%               o%   o           � G    

"   
 �           2    1� �   � B   �%               o%   o           � G    
"   
 �           �2    1� �   � �   �%               o%   o           o%   o           
"   
 	�            3    1� �   	� B   �%               o%   o           � �  ! 
"   
 �           t3    1� �   � B   �%               o%   o           � G    	
"   
 f�           �3    1� �   f� B   �%               o%   o           �    
"   
 f�           \4    1�   	 f� u   �%               o%   o           o%   o           
"   
 
�           �4    1�    
� �   �%               o%   o           %               
"   
 ��          T5    1� )   �� �     
"   
 �           �5    1� 7   � B   �%               o%   o           � K   

"   
 �           6    1� Z   � �  	 �%               o%   o           � G    
"   
 	�           x6    1� g   	� �  	 �%               o%   o           � G    
"   
 ��          �6    1� w   �� �     
"   
 ��          (7    1� �   �� �  	   
"   
 f�           d7    1� �   f� �   �o%   o           o%   o           %               
"   
 ��          �7    1� �   �� �     
"   
 ��          8    1� �   �� �  	   
"   
 ��          X8    1� �   �� �  	   
"   
 ��          �8    1� �   �� �  	   
"   
 ��          �8    1� �   �� �  	   
"   
 ��          9    1�    �� �  	   
"   
 ��          H9    1�    �� �     
"   
 	�           �9    1� /   	� B   �%               o%   o           � F  4 

"   
 ��          �9    1� {   �� �     
"   
 ��          4:    1� �   �� �     
"   
 ��          p:    1� �   �� �     
"   
 ��          �:    1� �   �� �  	   
"   
 ��          �:    1� �   �� �  	   
"   
 ��          $;    1� �   �� �  	   
"   
 ��          `;    1� �   �� �     
"   
 �           �;    1� �   � �  	 �%               o%   o           � G    
"   
 �           <    1� �   � �  	 �%               o%   o           � G    
"   
 
�           �<    1�    
� �  	 �%               o%   o           � G    
"   
 	�           �<    1�    	� �  	 �%               o%   o           � G    

"   
 
�           l=    1� .   
� �   �%               o%   o           %               
"   
 
�           �=    1� <   
� �   �%               o%   o           o%   o           
"   
 �           d>    1� N   � �   �%               o%   o           %               
"   
 �           �>    1� ^   � �   �%               o%   o           %               
"   
 �           \?    1� j   � �   �%               o%   o           o%   o           
"   
 �           �?    1� �   � �   �%               o%   o           %               
"   
 ��          T@    1� �   �� �  	   
"   
 �           �@    1� �   � �   �%               o%   o           %              
"   
 ��          A    1� �   �� �  	   
"   
 ��          HA    1� �   �� �  	   
"   
 ��          �A    1� �  
 �� �  	   
"   
 �           �A    1� �   � �  	 �%               o%   o           � .   

"   
 
�           4B    1� �   
� �  	 �%               o%   o           � G    
"   
    "    �%     start-super-proc ��%     adm2/smart.p ݔP �L 
�H T   %              �     }        �GG %              
"   
   �       TC    6� *     
"   
   
�        �C    8
"   
   �        �C    ��     }        �G 4              
"   
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout �
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �D    �� *   � P   �        �D    �@    
� @  , 
�        E    �� 3   �p�               �L
�    %              � 8      E    � $         � :          
�    � T   �
"   
 �p� @  , 
�       F    �� �   �p�               �L"    , �   � '   
� )   ��     }        �A      |    "      � '   
%              (<   \ (    |    �     }        �A� +   �A"    
    "    �"    
  < "    �"    
(    |    �     }        �A� +   �A"    

�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �G    �� *   � P   �        �G    �@    
� @  , 
�       H    �� 3   �p�               �L
�    %              � 8      H    � $         � :          
�    � T   �
"   
 �p� @  , 
�       $I    �� 7  
 �p�               �L"    , 
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �I    �� *   � P   �        �I    �@    
� @  , 
�       �I    �� 3   �p�               �L
�    %              � 8      �I    � $         � :          
�    � T   �
"   
 �p� @  , 
�       �J    �� �   �p�               �L
"   
 , 
�H T   %              �     }        �GG %              
"   
   
"   
 

"   
   
"   
   (�  L ( l       �        �K    �� *   � P   �        �K    �@    
� @  , 
�       �K    �� 3     p�               �L
�    %              � 8      �K    � $         � :          
�    � T     
"   
 �p� @  , 
�       �L    �� ]  
 �p�               �L%     SmartWindow 
"   
   p� @  , 
�       8M    �� t     p�               �L%      WINDOW  
"   
  p� @  , 
�       �M    �� ,    p�               �L%               
"   
  p� @  , 
�       �M    �� 
    p�               �L(        � G      � G      � G      �     }        �A
�H T   %              �     }        �GG %              
"   
 
 (   � 
"   
 �    �        �N    �� *   �
"   
   � 8      $O    � $         � :          
�    � T   �
"   
   �        |O    �
"   
   �       �O    /
"   
   
"   
   �       �O    6� *     
"   
   
�        �O    8
"   
   �        P    �
"   
   �       4P    �
"   
   p�    � T   	
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 �    �        �P    �A"    �A
"   
   
�        DQ    �@ � 
"   
 
"      �       }        �
"   
 �%              %                "    �%     start-super-proc ��%     adm2/appserver.p �	�    � �     
�    �     }        �%               %      Server  - �     }        �    "    � G    �%                   "    � G    �%      NONE    p�,  8         $     "            � �   �
�    
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        �S    �� *   � P   �        �S    �@    
� @  , 
�       �S    �� 3   �p�               �L
�    %              � 8      �S    � $         � :          
�    � T   �
"   
 �p� @  , 
�       �T    �� �   �p�               �L"    , p�,  8         $     "            � �   �
�     "    �%     start-super-proc ��%     adm2/visual.p ��   � %     � !     � #  C   
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
   (�  L ( l       �        V    �� *   � P   �         V    �@    
� @  , 
�       ,V    �� 3   �p�               �L
�    %              � 8      8V    � $         � :          
�    � T   �
"   
 �p� @  , 
�       HW    �� b   �p�               �L"    , � 
"    
 �%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP ݔ%     processAction   
�    %     CTRL-PAGE-DOWN  "    �%     start-super-proc �%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %      initializeDataObjects 0 0   A    �    � �   
�    � �   �A    �    � �     
�    � �   �%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents %     buildDataRequest ent0 A    �    � �   �
�    � �   
%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 �
"   
 �%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 (�  L ( l       �        �[    �� *   � P   �        �[    �@    
� @  , 
�       �[    �� 3   �p�               �L
�    %              � 8      �[    � $         � :   �     
�    � T   �
"   
 �p� @  , 
�       �\    �� w   �p�               �L
�             �G
�H T   %              �     }        �GG %              
"   
 �
"   
 �
"   
 �
"   
 �(�  L ( l       �        t]    �� *   � P   �        �]    �@    
� @  , 
�       �]    �� 3   �p�               �L
�    %              � 8      �]    � $         � :   �     
�    � T   �
"   
 �p� @  , 
�       �^    �� .   �p�               �L%              (        �     }        �G� �   �G� 
"   
 �
"   
   �        H_    �%              
"   
 �
"   
 ��     }        �%               
"   
 �%      CLOSE   %               �    }        ��      %     carga-grupos    �    }        �� &      �    }        �� &      �    }        ��      %     carga-grupos    �    }        �� &      "      � &         "      &    "      4   &    $    4          %              &    4       %              4       %              4       %              %     carga-usuarios  � 
"   
 �
"   
 

"   
 ��        �a    %%              
�     }        �
"   
   %     destroyObject       �     }        �    �  � 2  	   %               
"   
 �
�    %     createObjects    �     }        �%     initializeObject � �     }        �$    4         %              � |     %                   %              %                   "      %                  "      "      "     
4      
   "      
"   
   �        d    `"      
"   
   �        @d    `%              
"   
   �        td    
`%                   %              %                   "      %                  "      �            �'�            �'
�           >"      "      
4      
   "          "    
%               
"   
   �        �e    �g"      �            �'*    4       %                  "     &    z     "          %              %                   "      %                   "      �    "      � �     �    "      � �    T   "      "      � �     %              "      %              "    �&    &    *    "      %              D  (   %                  �            F� &    F�           �            F�              � &   �� &     �            B� &     � &     4       %                  "     &    "    �&    &    � �     *    "      �              "      "       (   %                  �            B� &    B�            B"      4       %                  "     &    z     "      "      4         %                  %              %                   "      %                  "      %       ,      %       ,      � &      "          %              %                   "      %                  "      %              %              � &      "      � &      "      � i    
%              � &    �%              %              "    �&    &    "    �&    &     "      (        "      %              %                   "      %              � &      � &      � &      t 8   P      ( "    ߱%                  "      %              4         "          4         "      %                  "      � &          %              %                   "      %                  "      �     "      �     "      A     "      T    "      "      %                   "    � �    �%               "  	  �� �      "    �� �      "      "      � |     "      "      "      � �          "    %              "      U    � �      %       x       � �   	       "    %              "      � �      � �          "    %              "      U    � �      %       x           "    � �    �"      %              � &      %              "  
    %              "      %              "      %                   "      "  
    %       
       *    "      %              4         "           "      %              "           "      %                   "      %              "      "      � &         "      &    "      4   &    $    4          %              &    4       %              4       %              4       %              %     cargar-treeview � &      ( X       "      %               ( (       "       %                   "       %              "       %              "       %              � �      %                  "       %              � �      %              %     carga-grupos    %              &    4       %                  " !   � &    �(        �     }        �G� �   �G� 
"   
 �
"   
   �     }        �
�    
"   
 �"    "    �
"   
 	� &         "      &    "      4   &    $    4          %              &    4       %              4       %              4       %              %              &    4       %              "      "      
"   
   %      CLOSE   %               %      SUPER   %     carga-usuarios  " "     8$    4         %       
       &    4       %       
           " "   	� �!   �    " "   	� �!   �8    " "   	� �!   �8    " "   	� �!   �8    " "   	� �!   �8    " "   	� �!  	 �T   %              " "     � �!   
%     loadDirectory   " "     " "       <   8    " $     � "   �     �    " $     � �!   �%              � "  B   8    " $   
� �!   �� J"  3   8    " $   
� �!   �� ~"     8    " $   � �!   �� �"     ( T    %              " %     ( T    %              " %     8    " %   
� �"   �    %              %                   " %     %                  " %     �     " %     �     " %     
�  T    " %     " %   ߱
" %  
   
" %  
 �
" %  
 �   �        " %     �  #     (0 \      �        �z    �%              (  (  �    
" %  
 f� #   �     � #   
�        �z    B� #   B�        �z    �
%   
               %              %                   " %     %                  " %     �     " %     �     " %     
�  T    " %     " %   ߱  �    
" %  
 �� #   

" %  
 � �        h|      � 
" % 
 
 

" %  
 ��        �|     � +#     
" %  
   �        �|    
" %  
      �        �|    %              
" %  
   
" % 
 
 �� T      (}     @    � @  , 
�       4}    �� 2#   �p�               �L� 6#   �L� 8#     
" %  
   �        �}    B
" %  
 
" % 
 
     @   �        �}    B� @  , 
�       �}    �� 2#     p�               �L
" %  
   �        H~    B
" % 
 
 �� @  , 
�       h~    �� 2#   �p�               �L� @#     
" %  
   
" % 
 
   " &     � \#     %     tvNodeaddOnExpand �" &     � h#     %     tvNodeSelect    " &     � o#  
   %     tvNodeCreatePopup �" &      �     }        ��  � z#  4   �  � �#     � �#  	   � �#     � �#     � �#     � �#  O   � H$     � W$     %      
            � d$  '   " &   �� �$  	   8    " &   	� �"   �� �$         " &   f� �$   �� �$  
       " &   � �$   �� �$     
" & 	 
   � 
" & 	 
 �     
" & 	 
 	     
�             �G8    " &   � �$   �%     tvNodeDropEnd   " &     " &     8    " '   � �$  	 �8    " '   � �$  
 �                �           �   l       ��                 �  �  �               �	                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �N     
                    � ߱              �  (  �      �N      4   �����N                �                      ��                  �  �                  t                       �  8  �  �  �  0O            �  �  `      �O      4   �����O                p                      ��                  �  �                  ��
                       �  �  �  o   �      ,                                 �  �   �  �O      �  �   �  �O      $  $  �  �  ���                        P     
                    � ߱        8  �   �   P      L  �   �  @P      `  �   �  `P          $   �  �  ���                       �P  @         |P              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                 �  .  �               ��
                    O   ����    e�          O   ����    R�          O   ����    ��      u                      �          �  $  �    ���                       �P     
                    � ߱                  �  �                      ��                                        �
                        4      4   ����Q      $    �  ���                       PQ     
                    � ߱        �      4  D      dQ      4   ����dQ      /    p                               3   ����xQ  �  �     �Q          O   ,  ��  ��  �Q                               , �                          
                               �      ��                            ����                                                        �   l       ��                      �               t
                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ���F                 �  �                                    O   ����    e�          O   ����    R�          O   ����    ��      �   �        �   �         �'    $  �   l      �e      4   �����e                �                      ��                  $  �                   �	                       $           �      �          h  P      ��                  )  6  �              t)                     )  |      $  t       ��                            7   ����         ����               f    �            �                  6   )        �  ����         �  f    �            �                                                        �e                 <  0                                   @                        O   ����  e�          O   ����  R�          O   ����  ��      \  $  *  �  ���                       4f                         � ߱          l      �  t  �F      D  ,      ��       0         +  4  \              �I      �f            +  �      $  +  �  ���                       Hf                         � ߱          $  +  �  ���                       xf                         � ߱            4   �����f      O   ����  e�          O   ����  R�          O   ����  ��      �  9   ,     �f           g         4g          @g             � ߱        �  $  -  �  ���                       �  A  0        T   ��         H                                             Tg                 �  �           `g           hg         �            p   �          1  �  H      pg      4   ����pg                �                      ��                  1  3                  �                       1  �  xg          �g             � ߱            $  2  X  ���                                     ,                      ��                  8  K                  �2                `     8  �  l    9  H  X      �g      4   �����g      �  :  �g      �  �   <  h      Lh  @         8h              � ߱        �  $   =  �  ���                       �	  $  >  	  ���                       Xh                         � ߱              �	      L                  ��                  @  J  4              �3                       @  0	      �	  (
       ��                            7   ����         ����               �h    �            x
                  6   @        �
  ����         �
  �h    �            x
                                                        dh                 �
  �
                                   @            �
   �
        O   ����  e�          O   ����  R�          O   ����  ��        A  B        �   ��         �                                             �h                 �  �           �h           �h         �            �   �    \  $  C  0  ���                       �h                         � ߱        �    D  x  �      �h      4   �����h      $  D  �  ���                       �h                         � ߱        �  �   F  �h            G    �      i      4   ����i                �                      ��                  G  I                  ܽ
                       G     hi  @         Ti              � ߱            $   H  �  ���                             p                �  �      ��                  _  �  �              �                       _  �      �  �       ��                            7   ����         ����               �i    �            <                  6   _        l  ����         `  �i    �            <                                                        ti                 �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      h  $  `  <  ���                       �i                         � ߱        �  $  a  �  ���                       �i                         � ߱        �  $  b  �  ���                       �i                         � ߱        	  �      �  T                      ��        0    	     e  g                  H�	      xj          e        $  e  �  ���                       �i                         � ߱        D  $  e    ���                       (j                         � ߱            4   ����Pj      $  f  �  ���                       �j          �j             � ߱        
  (      �  �                      ��        0    
     h  k                  ��	      $k     �     h  �      $  h  T  ���                       �j                         � ߱        �  $  h  �  ���                       �j                         � ߱            4   �����j  @  $  i    ���                       8k          Dk             � ߱            $  j  l  ���                       Pk          \k             � ߱        hk          tk         �k          �k         �k                         � ߱        (  $  l  �  ���                       L  A  q        �   ��         x                                             �k                 �  �           �k           �k         �            �   �          \      �  �F      �  �      ��                  s  �  �              �c                       s  �      �  �       ��                            7   ����          ��                     �            (                  6   s        X   ��         L        �            (                                                        �k                 �  �           �k           �k                      t   �        O   ����  e�          O   ����  R�          O   ����  ��      T  $  t  (  ���                       �k                         � ߱        �  $  u  �  ���                       l                         � ߱          $  v  �  ���                       Hl                         � ߱        \  $  x  0  ���                       pl                         � ߱        �  $  y  �  ���                       |l       
       
           � ߱          $  z  �  ���                       �l                         � ߱        d  $  |  8  ���                       �l       	       	           � ߱        �      �  �      Hm      4   ����Hm                x                      ��                  �  �                  f                       �  �    �      �  H                      ��        0         �  �                  tf      �m     �     �        $  �  �  ���                       hm                         � ߱        8  $  �    ���                       �m                         � ߱            4   �����m        �  d  t      �m      4   �����m      O   �  �� ��          $  �  �  ���                       0n       	       	           � ߱        h    �           Dn      4   ����Dn      $  �  <  ���                       dn       	       	           � ߱        �    �  �  �      xn      4   ����xn      O   �  �� ��        $  �  �  ���                       �n                         � ߱        (%  p   �  �n         �  \  �     �n                �                      ��                  �  �                  ܉                       �  0    $  �  �  ���                       �n       
       
           � ߱            $  �  0  ���                       �n                         � ߱        �   �     �n                �                      ��                  �  �                  `�                       �  l  @   $  �     ���                       �n       
       
           � ߱        �   $  �  l   ���                       �n                         � ߱            $  �  �   ���                       �n                         � ߱        X"   !     �n        �  !  �!       o      4   ���� o                �!                      ��                  �  �                  ܻ	                       �  ,!   "  $  �  �!  ���                       (o       
       
           � ߱            $  �  ,"  ���                       4o                         � ߱        �#  h"     \o        �  �"   #      ho      4   ����ho                #                      ��                  �  �                  @�	                       �  �"  h#  $  �  <#  ���                       �o       
       
           � ߱            $  �  �#  ���                       �o                         � ߱            �#     �o        �  �#  h$      �o      4   �����o                x$                      ��                  �  �                  ļ	                       �  �#  �$  $  �  �$  ���                       �o       
       
           � ߱            $  �  �$  ���                       �o                         � ߱        �'    �  D%  �%      p      4   ����p                �%                      ��                  �  �                  `�	                       �  T%  �&  9   �     0p          <p         Pp          \p         pp          |p         �p          �p         �p          �p         �p          �p             � ߱        �&  $  �  �%  ���                             �  �&  �&      q      4   ����q      $  �  '  ���                       q          q             � ߱        ,q          Hq         pq          |q         �q                         � ߱            $  �  H'  ���                       l)  s   �  (                 h)              0(  �(       ��                            7   ����           ��                     �            �(                  6   �         �(   ��                    �            �(                                                                <)  0)                                   @            )    )          �q  �q          P)  �+  s   �  �)      h+                      �)  *  �*                               7   ����          ����                r   �            d*                  6   �         �*  ����               r   �            d*                                                                �*  �*                                   @            �*   �*        J   �        ��L+    ��                                                          \r  �r  �r                      4+                 �q   �q   r      /   �  �+     �+                          3   �����r            �+                      3   �����r                T,                                              ]PD          p?  �A  ppp,             �     ��     �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ,0  @  P  `  Xhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������		(	8	H	X	h	x	�	�	�	�	�	�	�	�	

(
8
H
X
h
x
�
�
�
�
�
�
�
�
(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������    ,0  @  P  `  Xhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������			(	8	H	X	h	x	�	�	�	�	�	�	�	�


(
8
H
X
h
x
�
�
�
�
�
�
�
�(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx�������� �T� $   / ? O _ o  � � � � � � � �                                                                                                                                                                                                                                                                                                            �          ��                             ��                             ��                              ��        �                   ��                             ��                             ��                            ����                                      =   �         =   4     �        2                 S�    s       2                 S�                    �           �   l       ��                 �    �               $
                    O   ����    e�          O   ����    R�          O   ����    ��      0  �   �                   l                      ��                  �  �                  X
                     �  �                             � ߱            $   �  @   �                                   �  �      x  `      ��                  �    �               c
                �     �  �      @  �       ��                            7   ����           ��                     �            �                  6   �            ��                    �            �                                                                L  @                                   @                0        O   ����  e�          O   ����  R�          O   ����  ��            �  �  @      �r      4   �����r                P                      ��                  �                    ��
                       �  �  �  9   �     xs          �s         �s          �s         �s          �s             � ߱        �  $  �  `  ���                                  �      �s      4   �����s                �                      ��                                       @�
                             t          t             � ߱            $    �  ���                       ,  /                                      3   ���� t      s   	  X                              �  �  �                               7   ����           ��                Pt   �            $                  6   	         H   ��               Pt   �            $                                                                �  �                                   @            d   t        J   	        ��    ��                                                          Xt                      �                 <t    ��                              ��        �                   ��                            ����                                =        �        2                 S�                    �           �   l       ��                   +  �               �Y
                    O   ����    e�          O   ����    R�          O   ����    ��      !   !                   �                  �   t      |t      4   ����|t                                        ��                    &                  ��	                                      !  �          �  �    �                                        !     ��                            ����                                            �           �   l       ��                  1  >  �               L�	                    O   ����    e�          O   ����    R�          O   ����    ��           ;  �   �       �t      4   �����t      n   <     �          �t        =    ,      �t      4   �����t      �   =  �t    ��                            ����                                            �           �   l       ��                  D  T  �               D�	                    O   ����    e�          O   ����    R�          O   ����    ��      u  �          u  �              � ߱        p  Z   N  �    �        u                  �               �              �              �              �              � ߱        �  h   P     �        (u              �  s   R  �       �                      �  D                                 7   ����          ����                hu   �            �                  6   R         �  ����               hu   �            �                                                                   �                                   @            �   �        J   R        ��|    ��                                                          �u  �u  �u                      d                 4u   @u   \u  �  s   R  �      �                        X  (                               7   ����           ��                ,v   �            �                  6   R         �   ��               ,v   �            �                                                                                                     @            �   �        J   R        ���    ��                                                          4v                      x                 v  D  s   R  �                @                X       ��                            7   ����           ��                     �            �                  6   R         �   ��                    �            �                                                                                                     @            �   �          Xv  dv          (      
   S  �� `             pv    ��                              ��        �                  ����                            s        2                 S�    �       2                 S�    �       2                 S�                    �           �   l       ��                  Z  d  �               <k                    O   ����    e�          O   ����    R�          O   ����    ��      �     a  |v  }          O   b  ��  ��  �v    ��                            ����                                            �           �   l       ��                  j  y  �               0k                    O   ����    e�          O   ����    R�          O   ����    ��      �   /   s  �                                 3   �����v      /   w                                   3   �����v    ��                            ����                                            H          �   l       ��                     �               �k                    O   ����    e�          O   ����    R�          O   ����    ��      V!   "                   �                X                  �  �      ��                  �  �                lk                ,     �  �       �  �  h                                7   ����          ��          	     �v    �            $                  6   �        T   ��        	 H  �v    �            $                                                        �v                 �  �                                   @            p   �          �                 �                                                                                                                                                              J   �          �    ��                                                           w                      �              O   ����  e�          O   ����  R�          O   ����  ��      �    �  H  �      8w      4   ����8w                                        ��                  �  �                  �k                       �  X  |    �  �  l      Xw      4   ����Xw                                        ��                  �  �                  |k                       �     $    �  �        xw      4   ����xw                                        ��                  �  �                  �k                       �  �  �    �  @  �      �w      4   �����w                                        ��                  �  �                  D k                       �  P  t    �  �  d      �w      4   �����w                                        ��                  �                    � k                       �  �          �        �w      4   �����w                                      ��                                      !k                         �  t  $    H  ���                       �w      "                   � ߱            /     �     �                          3   ����,x  �        �                      3   ����Hx             	                      3   ����Tx              #  x	                                             "  0
          �	  
    � �	                                                                                                                      	     0   @   P   `   p   �      	     0   @   P   `   p   �          " #   ��                             ��                            ����                                            �           �   l       ��                    <  �               |"k                    O   ����    e�          O   ����    R�          O   ����    ��      V!   $                   �               *  �         `x      4   ����`x      O   ,  ��  ��  �x  d    1  <  L      �x      4   �����x      O   2  ��  ��  �x  �    5  �  �      �x      4   �����x      O   6  ��  ��  y        9  �  �      (y      4   ����(y      O   :  ��  ��  Hy             $ 	 �          �  �  $ �                                                                                                                              
 $   4   D   T   d   t   �   �      
 $   4   D   T   d   t   �   �              $     ��                            ����                                                      �   l       ��                 B  �  �               0k                    O   ����    e�          O   ����    R�          O   ����    ��      �"   %    �              �          V!   %                   �          \  $ Q  0  ���                       Ty      %                   � ߱        �  $ R  �  ���                       �y      %                   � ߱              V  �  L  �  �y      4   �����y                                        ��                  V  g                  �9k                       V  �                D                      ��                  j  �                  �=k                       j  \    T      �                        ��        0         l  t                  �>k    %  Lz     �     l  �      $  l  �  ���                       �y      %                   � ߱          $  l  �  ���                       �y      %                   � ߱            4   ����$z  l  $ m  @  ���                       `z     
 %                   � ߱            $  n  �  ���                       �z      %                   � ߱        �  $  �  �  ���                       x{     
 %                   � ߱          �      �  X                      ��        0         �  �                  ?k    %  |            �        $  �  �  ���                       �{      %                   � ߱        H  $  �    ���                       �{      %                   � ߱            4   �����{  �  $ �  �  ���                        |     
 %                   � ߱        �    �  �  �      D|      4   ����D|      O   �  �� ��      8    �           t|      4   ����t|      O   �  �� ��      |    �  T  d      �|      4   �����|      O   �  �� ��      P	  p   �  �|  �  �  �  �       �|                $                      ��                  �  �                  �@k                       �  �  |  $   �  P  ���                       �|  @         �|              � ߱            �   �  @}          �     �}      $  �  �  ���                       �}  @         �}              � ߱            $  �  $	  ���                       t~  @         T~              � ߱        h	    �  �~     �~  x	  �   �  �~      O   �  �� ��                 % 
 �
          T
  |
  ( � �	                                                                                  
                                         
              (   8   H   X   h   x   �   �   �       (   8   H   X   h   x   �   �   �        �  �  %     ��                            ����                                                      �   l       ��                 �  �  �                Bk                    O   ����    e�          O   ����    R�          O   ����    ��      �"   &    �              �          V!   &                   �              p   �  �~     8  �  �  0     �~      /   �  \     l                          3   �����~            �                      3   ����    �           /   �  �     �                          3   ����(                                  3   ����D  �  �     P                �                      ��                  �  �                  $Nk                       �  (    /  �  �     �                          3   ����\                                   3   ����|        �  ,  <  T  �      4   �����      O   �  ��  ��  �      	  �  �                                    �  3   �����      3   �����    �     �  �  �  �  �      	   �  �                                          3   �����  �       �      	  �  P                                    `  3   �����  p  3   �����      3   ����(�      �     H�                                      ��                  �  �                  Ok                       �  �  P    �  (  8      T�      4   ����T�      O   �  ��  ��  t�  �    �  l  |      ��      4   ������      O   �  ��  ��  ��       �  �  ,      ��      4   ������                <                      ��                  �  �                  �Ok                       �  �  �  �  �  ̀      T         
   t  �                  3   ����؀      $   �  �  ���                               
 & 	       	           � ߱              �  �        �      4   �����      O   �  ��  ��  �      O   �  ��  ��  �        �  T  d      $�      4   ����$�      /   �  �     �                          3   ����D�  �        �                      3   ����`�            �                      3   ����l�             & 	 �          �  �  $ � 4                                                                                                              
             
 $   4   D   T   d   t   �   �      
 $   4   D   T   d   t   �   �          �   &     ��                              ��        �                  ����                                            �           �   l       ��                 �  ]  �               �Rk                    O   ����    e�          O   ����    R�          O   ����    ��      V!   '                   �          �      �   t      x�      4   ����x�                                        ��                    5                  ak                                 9  �        ��      4   ������                                        ��                  9  Z                  hak                       9  �             ' 
 @          �    ( � `                                                                                                                                           (   8   H   X   h   x   �   �   �       (   8   H   X   h   x   �   �   �              '     ��                            ����                                ��          �  �   �P                              
 �                                                                 �   X     �  P     �	+%                                    
 �                                                                �   X     �         1%                                    
 �                                                                �   X     �  P       8%                                      �                                                                                                                                        g          �  d   �\  �  P                      
 �                                                                 �   X     �       J%                                    
 �                                                                �   X     �  x     sR%                                    
 �                                                                �   X     �  
       e%                                      �                                                                                                                                                  6 �/�          s  �
   ��  |  �                      
 �                                                                    X     �       �1%                                    
 �                                                                   X     �  2     �	l%                                    
 �                                                                   X     �         %                                    
 �                                                                   X     �  P       �%                                      �                                                                                                                                                                   �	   d d     4   ��1�	1  � �                                               �                                                                         d     D                                                                 H  � � g                                 �          �          h  � w  M                                                        [     �     �%               H  F� ��                                �          �          P   ��=d                                                           �%  G     x  ���l                                                        C     �             "  
       T  T       H  � >�/�                                s          �            D                                                                    TXS appSrvUtils tGrupoAccesos Tabla de Reportes Task-No Llave-C Campo-D Campo-F Campo-I Campo-C Llave-I Llave-F Llave-D Campo-L tUsuarioGrupos tUsuarios ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST x-filtro-menu <Todos> x-tGrupoAccesos wWin COMBO-BOX-modulo Item 1 TOGGLE-usuarios-activos BROWSE-2 GRUPO DE ACCESOS DEL MODULO X(8) X(50) X(80) BROWSE-6 Usuarios X(15) X(120) X(10) BROWSE-7 X(6) fMain yes/no X(256) GUI Perfil de Usuario DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-6 TOGGLE-usuarios-activos BROWSE-7 COMBO-BOX-modulo BROWSE-2 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE GENERAL  iStartPage ADM-ERROR celda_br cual_celda n_cols_browse col_act t_col_br vg_col_eti_b PROCESO ADM-CREATE-OBJECTS x-grupos x-sec PF-G004 Aplicaciones por Usuario , PF-G003 Aplicaciones <Inexistente> OpcMnu NumOpc LenOpc H-parent H-label Nivel-Act Nivel-Ant s-seguridad s-aplic-id s-user-id OK s-codmenu s-label s-programa i MAIN PF-G002 Opciones de Men�s ADMIN NO CREAR NADA SUB-MENU LINEA - SEPARADOR                     CAJA-MARCADOR * CARGA-GRUPOS _User ACTIVO INACTIVO CARGA-USUARIOS pParent CARGAR-TREEVIEW DISABLE_UI ENABLE_UI EXITOBJECT INITIALIZEOBJECT pcnodeKey x-aplic-id x-icon x-expanded x-label nCust norder cSalesrep icustnum iorder cFullPath n222 n2222 sr= cust= order= fileName= = TVNODEADDONEXPAND ccustname cparentKey n Add a child node,MenuAddChildNode,RULE,,Hello World,MenuHelloWorld Add Salesrep,MenuAddSR,Add Customer,MenuAddCustomer Add order,MenuAddOrder Add order line,MenuAddOrderLine TVNODECREATEPOPUP pcEvent mouseX mouseY cWidgets hWidget icount k targetKe   SCREEN-VALUE SCREEN-VALUE= ? hNodeBuffer EDITOR lab 
 FILL-IN VALUE-CHANGED TVNODEDROPEND addOnExpand select rightClick tvNodeCreatePopup failed with the following message: MenuAddChildNode MenuAddSR MenuAddCustomer MenuAddOrder MenuAddOrderLine addToEdMsg('Menu item event fired: ' + pcEvent + ' for key ' + pcnodeKey + '
') MenuHelloWorld Hello World! Node key parent of the popup menu item: DragBegin dropOnYourself n4 cancelDrag n1 hTargetFrame getOtherWinTargetFrame DropEnd, TVNODEEVENT optn MoreCust= MoreOrder= TVNODESELECT REPO01 REPO02 REPO03 Grupo Modulo Nombre del Modulo Usuario Nombre y Apellidos Estado Descripcion Modulo Cod.Menu Opcion del Menu Mostrar solo usuarios activos Modulos IDX01 _Userid �  �/    p6      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
 pcProcName  �   ��      �         pcProcName      ��              
 pcProcName      ��      $        piPageNum       ��      H        piPageNum       ��      l        pcPageList      ��      �        pcProc  �  ��      �        pcLinkName      ��      �        pcLinkName    ��      �       
 phTarget        ��              phTarget        ��      @        piPageNum       ��      d        pcValue     ��      �        piPageNum       ��      �        pcAction        ��      �       
 phAppService        ��      �        pcMode     ��             
 phSource    D  ��      8        phSource        ��      \       
 phSource    �  ��      �        pcText  �  ��      �        pcText      ��      �        pcText  �  ��      �       
 phObject      ��             
 phObject        ��      (        phObject        ��      L        pcField     ��      l        pcCursor    �  ��      �       
 phCaller    �  ��      �        phCaller    �  ��      �        phCaller        ��      �        phCaller    (  ��               pcMod   H  ��      @        pcMod       ��      `       
 pcMod   �  ��      �       
 phSource    �  ��      �        phSource        ��      �       
 phSource    �  ��      �        pdRow       ��              pdRow       ��      ,       
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   �	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    �               ,  .  H  �     9                                   �  �  	     :                                   �  �  �  L	     ;                                       	  �	     <                                   A  T	  �	     =                                   S  T  �	  �	     >                                   _  `  a  �	  ,
     ?                                   l  m  n  p  �	  l
     @                                   y  z  {  |  ~  <
  �
     A                                   �  �  �  �  �
  �
     B                                   �  �  �
  (     C                                   �  �  �  �  �
  h     D                                   �  �  8  �     E                                   �  �  �  �  �  �  �  �  p       F               �                  adm-create-objects    (             x-grupos    D        <     x-sec   `     ,X     OpcMnu  |       t     NumOpc  �       �     LenOpc  �       �     H-parent    �       �     H-label �       �     Nivel-Act               Nivel-Ant   4        (     s-seguridad T        H     s-aplic-id  t        h     s-user-id   �     	   �     OK  �     
   �     s-codmenu   �        �     s-label �        �     s-programa            �     i   �  @  c   G             0                  carga-grupos         $  )  *  +  ,  -  0  1  2  3  4  6  8  9  :  <  =  >  @  B  C  D  F  G  H  I  J  K  _  `  a  b  e  f  g  h  i  j  k  l  q  s  t  u  v  x  y  z  |    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �          H               �                  carga-usuarios  �  �  �  �  �  �  �  �               	        !      d        pParent �  �     I       L      �                  cargar-treeview   &  +  l  �     J               �                  disable_UI  ;  <  =  >  �  @     K               4                  enable_UI   N  P  R  S  T    �     L               �                  exitObject  a  b  d  T  �     M               �                  initializeObject    s  w  y    #           x-aplic-id  (  #           x-icon  H  #     <     x-expanded  d  #     \     x-label �  "      x     nCust   �  "      �     norder  �  "      �     cSalesrep   �  "      �     icustnum    �  "      �     iorder      "           cFullPath       "      0        pcnodeKey   �  �     N   �        l                  tvNodeAddOnExpand   �  �  �  �  �  �  �  �  �  �  �              �  $      �     nCust   �  $      �     norder    $           cSalesrep   <  $      0     icustnum    \  $      P     ccustname   x  $      p     iorder      $   	   �     cparentKey      $      �        pcnodeKey   <     	   O   �  �      �                  tvNodeCreatePopup   *  ,  1  2  5  6  9  :  <  @  %      8     mouseX  \  %      T     mouseY  |  %      p     cWidgets    �  %      �  
   hWidget �  %      �     icount  �  %   	   �     targetKe        %   
   �  
   hNodeBuffer   %              pcEvent     %      ,        pcnodeKey   �  x     P   $  �      h                  tvNodeDropEnd   Q  R  V  g  j  l  m  n  t  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    &            nCust   $  &           norder  D  &      8     cSalesrep   d  &      X     icustnum    �  &      x     iorder      &   	   �  
   hTargetFrame    �  &      �        pcEvent     &      �        pcnodeKey   8  $     Q   �  �                        tvnodeEvent �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  '      �     nCust   �  '      �     norder  �  '      �     cSalesrep   �  '      �     icustnum      '           ccustname   0  '      (     iorder  P  '   	   D     cparentKey      '   
   d     optn        '      �        pcnodeKey   �  �     R   |  l      �                  tvNodeSelect      5  9  Z  ]  �  L#       �      p"                      �  $  4  
   tGrupoAccesos   �         �         �         �        �        �        �        �         �         �        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L �      
   tUsuarioGrupos  �         �         �         �        �        �        �        �         �         �        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L     �     
   tUsuarios   x         �         �         �        �        �        �        �         �         �        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L �          �  
   appSrvUtils        �     x-filtro-menu   (          
   wWin    P       <     COMBO-BOX-modulo    |       d     TOGGLE-usuarios-activos �        �  
   gshAstraAppserver   �        �  
   gshSessionManager   �  	 	     �  
   gshRIManager      
 
       
   gshSecurityManager  @        ,  
   gshProfileManager   l        T  
   gshRepositoryManager    �        �  
   gshTranslationManager   �        �  
   gshWebManager   �        �     gscSessionId            �     gsdSessionObj   (          
   gshFinManager   L        <  
   gshGenManager   p        `  
   gshAgnManager   �        �     gsdTempUniqueID �        �     gsdUserObj  �        �     gsdRenderTypeObj             �     gsdSessionScopeObj              
   ghProp  @        4   
   ghADMProps  d        T   
   ghADMPropsBuf   �        x      glADMLoadFromRepos  �     	   �      glADMOk �     
   �   
   ghContainer �        �      cObjectName !       �      iStart  $!       !     cAppService D!       8!     cASDivision p!       X!     cServerOperatingMode    �!       �!     cFields �!       �!     iStartPage  �!     � �!  
   celda_br    �!       �!  
   cual_celda  "        "     n_cols_browse   ,"       $"     col_act L"       @"     t_col_br             `"     vg_col_eti_b    �"    \  �"  tGrupoAccesos   �"    \  �"  tUsuarioGrupos  �"    \  �"  tUsuarios   �"    B  �"  x-tGrupoAccesos #       �"  PF-G004 #       #  PF-G003 4#       ,#  PF-G002           D#  _User            C   �  �  �  �  �  �  �  �  �  �  w  x  y  z  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  n	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  �	  j
  u
  v
  x
  y
  z
  {
  |
  }
  ~
  
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
                               !  "  #  %  &  '  (  )  *  ,  -  .  /  0  1  2  3  4  5  6  7  8  :  ;  <  =  >  ?  @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  �  �                    '  C  U  z  �  �  �  5  M  N  h  x  y  z  }  ~    �  �  �  �  �  m  n  r  |  �  �  �  �  �            &  ,  2  5  :  >  ?  @  B  F  �  �      N  [  j  w  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �           H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i �'  f!  C:\Progress\OpenEdge\src\adm2\containr.i �'  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �'  ��  C:\Progress\OpenEdge\src\adm2\visual.i   0(  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  d(  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    �(  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �(  I�  C:\Progress\OpenEdge\src\adm2\smart.i     )  Ds   C:\Progress\OpenEdge\gui\fn  T)  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   |)  Q.  C:\Progress\OpenEdge\gui\set �)  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �)  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    *  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    \*  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  �*  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �*  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i +  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i T+  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    �+  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �+  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i ,  �j  C:\Progress\OpenEdge\gui\get D,  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    l,  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �,  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �,  Su  C:\Progress\OpenEdge\src\adm2\globals.i  (-  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i \-  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   �-  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �-  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   .  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  \.  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  �.  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �.  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    /  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   P/  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   �/  �   d:\newsie\on_in_co\APLIC\adm\perfiles-erp-progress.w     �  �       0     �  $   0    �       0  �   �     00     �     @0  �   �     P0     �     `0  �   �     p0     K  #   �0  �   5     �0     3      �0  �   ,     �0     *      �0  �   )     �0     '      �0  r        �0  n   �      1     �  "   1  i   �      1     t     01  P   [     @1  �   R     P1     �  !   `1  �   �     p1     �     �1  �   �     �1     �     �1  �   �     �1     �     �1  g   r     �1     S     �1  O   ;     �1  �   �      2     �      2  �   �      2     ;     02  �   0     @2          P2  �        `2     �     p2  �   �     �2     �     �2  �   �     �2     �     �2  �   �     �2     r     �2  �   o     �2     M     �2  }   A      3          3     �      3     U     03          @3  7   �     P3  �   �     `3  O   �     p3     �     �3     U     �3  �        �3  �        �3  O   �
     �3     �
     �3     �
     �3  �   r
     �3  x   j
  
    4  M   U
     4     D
      4     �	     04  a   �	  
   @4  �  �	     P4     �	     `4  �  n	     p4  O   `	     �4     O	     �4     	     �4  �   +     �4     �     �4     R     �4  x   L     �4     3     �4     �      5     �     5     �      5     �     05  Q   {  
   @5          P5     �  
   `5     �     p5     �  
   �5  f   �     �5     /  	   �5  "   �     �5     �     �5     �     �5  Z   e     �5     m     �5     .      6          6            6     �     06  3   �       @6     L      P6     !       `6           