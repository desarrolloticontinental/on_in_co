	��V��Pb�6  ��              �                                P2 36D40114utf-8 MAIN D:\xpciman\progress\Menu-ERP-Progress\perfiles-de-usuarios-erp-progress.w,, PROCEDURE tvNodeSelect,,INPUT pcnodeKey CHARACTER PROCEDURE tvnodeEvent,,INPUT pcEvent CHARACTER,INPUT pcnodeKey CHARACTER PROCEDURE tvNodeDropEnd,,INPUT pcEvent CHARACTER,INPUT pcnodeKey CHARACTER PROCEDURE tvNodeCreatePopup,,INPUT pcnodeKey CHARACTER PROCEDURE tvNodeAddOnExpand,,INPUT pcnodeKey CHARACTER PROCEDURE initializeObject,, PROCEDURE get-usuario-opciones-menu,,INPUT pUser CHARACTER PROCEDURE exitObject,, PROCEDURE excel-perfil-todos-los-usuarios,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE cargar-treeview,,INPUT pParent CHARACTER PROCEDURE carga-usuarios,, PROCEDURE carga-grupos,, PROCEDURE adm-create-objects,, PROCEDURE start-super-proc,,INPUT pcProcName CHARACTER PROCEDURE adm-clone-props,, PROCEDURE toggleData,,INPUT plEnabled LOGICAL PROCEDURE showMessageProcedure,,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL PROCEDURE returnFocus,,INPUT hTarget HANDLE PROCEDURE repositionObject,,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL PROCEDURE removeLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE removeAllLinks,, PROCEDURE modifyUserLinks,,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE PROCEDURE modifyListProperty,,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER PROCEDURE hideObject,, PROCEDURE editInstanceProperties,, PROCEDURE displayLinks,, PROCEDURE createControls,, PROCEDURE changeCursor,,INPUT pcCursor CHARACTER PROCEDURE applyEntry,,INPUT pcField CHARACTER PROCEDURE adjustTabOrder,,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER PROCEDURE addMessage,,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER PROCEDURE addLink,,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE PROCEDURE unbindServer,,INPUT pcMode CHARACTER PROCEDURE startServerObject,, PROCEDURE runServerObject,,INPUT phAppService HANDLE PROCEDURE restartServerObject,, PROCEDURE initializeServerObject,, PROCEDURE disconnectObject,, PROCEDURE destroyServerObject,, PROCEDURE bindServer,, PROCEDURE processAction,,INPUT pcAction CHARACTER PROCEDURE enableObject,, PROCEDURE disableObject,, PROCEDURE applyLayout,, PROCEDURE viewPage,,INPUT piPageNum INTEGER PROCEDURE viewObject,, PROCEDURE toolbar,,INPUT pcValue CHARACTER PROCEDURE selectPage,,INPUT piPageNum INTEGER PROCEDURE removePageNTarget,,INPUT phTarget HANDLE,INPUT piPage INTEGER PROCEDURE passThrough,,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER PROCEDURE notifyPage,,INPUT pcProc CHARACTER PROCEDURE initPages,,INPUT pcPageList CHARACTER PROCEDURE initializeVisualContainer,, PROCEDURE hidePage,,INPUT piPageNum INTEGER PROCEDURE destroyObject,, PROCEDURE deletePage,,INPUT piPageNum INTEGER PROCEDURE createObjects,, PROCEDURE constructObject,,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE PROCEDURE confirmExit,,INPUT-OUTPUT plCancel LOGICAL PROCEDURE changePage,, PROCEDURE assignPageProperty,,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER FUNCTION Signature,CHARACTER,INPUT pcName CHARACTER FUNCTION showmessage,LOGICAL,INPUT pcMessage CHARACTER FUNCTION setUserProperty,LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION setUIBMode,LOGICAL,INPUT pcMode CHARACTER FUNCTION setTranslatableProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setSupportedLinks,LOGICAL,INPUT pcLinkList CHARACTER FUNCTION setRunAttribute,LOGICAL,INPUT cRunAttribute CHARACTER FUNCTION setPhysicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setPhysicalObjectName,LOGICAL,INPUT cTemp CHARACTER FUNCTION setPassThroughLinks,LOGICAL,INPUT pcLinks CHARACTER FUNCTION setParentDataKey,LOGICAL,INPUT cParentDataKey CHARACTER FUNCTION setObjectVersion,LOGICAL,INPUT cObjectVersion CHARACTER FUNCTION setObjectParent,LOGICAL,INPUT phParent HANDLE FUNCTION setObjectName,LOGICAL,INPUT pcName CHARACTER FUNCTION setLogicalVersion,LOGICAL,INPUT cVersion CHARACTER FUNCTION setLogicalObjectName,LOGICAL,INPUT c CHARACTER FUNCTION setInstanceProperties,LOGICAL,INPUT pcPropList CHARACTER FUNCTION setDynamicObject,LOGICAL,INPUT lTemp LOGICAL FUNCTION setDesignDataObject,LOGICAL,INPUT pcDataObject CHARACTER FUNCTION setDBAware,LOGICAL,INPUT lAware LOGICAL FUNCTION setDataTargetEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setDataTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setDataSourceNames,LOGICAL,INPUT pcSourceNames CHARACTER FUNCTION setDataSourceEvents,LOGICAL,INPUT pcEventsList CHARACTER FUNCTION setDataSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDataLinksEnabled,LOGICAL,INPUT lDataLinksEnabled LOGICAL FUNCTION setContainerSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setContainerSource,LOGICAL,INPUT phObject HANDLE FUNCTION setContainerHidden,LOGICAL,INPUT plHidden LOGICAL FUNCTION setChildDataKey,LOGICAL,INPUT cChildDataKey CHARACTER FUNCTION reviewMessages,CHARACTER, FUNCTION propertyType,CHARACTER,INPUT pcPropName CHARACTER FUNCTION messageNumber,CHARACTER,INPUT piMessage INTEGER FUNCTION mappedEntry,CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER FUNCTION linkProperty,CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER FUNCTION linkHandles,CHARACTER,INPUT pcLink CHARACTER FUNCTION instancePropertyList,CHARACTER,INPUT pcPropList CHARACTER FUNCTION getUserProperty,CHARACTER,INPUT pcPropName CHARACTER FUNCTION getUIBMode,CHARACTER, FUNCTION getTranslatableProperties,CHARACTER, FUNCTION getSupportedLinks,CHARACTER, FUNCTION getRunAttribute,CHARACTER, FUNCTION getQueryObject,LOGICAL, FUNCTION getPropertyDialog,CHARACTER, FUNCTION getPhysicalVersion,CHARACTER, FUNCTION getPhysicalObjectName,CHARACTER, FUNCTION getPassThroughLinks,CHARACTER, FUNCTION getParentDataKey,CHARACTER, FUNCTION getObjectVersionNumber,CHARACTER, FUNCTION getObjectVersion,CHARACTER, FUNCTION getObjectParent,HANDLE, FUNCTION getObjectPage,INTEGER, FUNCTION getObjectName,CHARACTER, FUNCTION getObjectInitialized,LOGICAL, FUNCTION getObjectHidden,LOGICAL, FUNCTION getLogicalVersion,CHARACTER, FUNCTION getLogicalObjectName,CHARACTER, FUNCTION getInstanceProperties,CHARACTER, FUNCTION getDynamicObject,LOGICAL, FUNCTION getDesignDataObject,CHARACTER, FUNCTION getDBAware,LOGICAL, FUNCTION getDataTargetEvents,CHARACTER, FUNCTION getDataTarget,CHARACTER, FUNCTION getDataSourceNames,CHARACTER, FUNCTION getDataSourceEvents,CHARACTER, FUNCTION getDataSource,HANDLE, FUNCTION getDataLinksEnabled,LOGICAL, FUNCTION getContainerType,CHARACTER, FUNCTION getContainerSourceEvents,CHARACTER, FUNCTION getContainerSource,HANDLE, FUNCTION getContainerHidden,LOGICAL, FUNCTION getContainerHandle,HANDLE, FUNCTION getChildDataKey,CHARACTER, FUNCTION fetchMessages,CHARACTER, FUNCTION assignLinkProperty,LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER FUNCTION anyMessage,LOGICAL, FUNCTION setServerOperatingMode,LOGICAL,INPUT pcServerOperatingMode CHARACTER FUNCTION setServerFileName,LOGICAL,INPUT pcFileName CHARACTER FUNCTION setASUsePrompt,LOGICAL,INPUT plFlag LOGICAL FUNCTION setASInitializeOnRun,LOGICAL,INPUT plInitialize LOGICAL FUNCTION setASInfo,LOGICAL,INPUT pcInfo CHARACTER FUNCTION setASHandle,LOGICAL,INPUT phASHandle HANDLE FUNCTION setASDivision,LOGICAL,INPUT pcDivision CHARACTER FUNCTION setAppService,LOGICAL,INPUT pcAppService CHARACTER FUNCTION runServerProcedure,HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE FUNCTION getServerOperatingMode,CHARACTER, FUNCTION getServerFileName,CHARACTER, FUNCTION getASUsePrompt,LOGICAL, FUNCTION getASInitializeOnRun,LOGICAL, FUNCTION getASInfo,CHARACTER, FUNCTION getASHasStarted,LOGICAL, FUNCTION getASHandle,HANDLE, FUNCTION getAsDivision,CHARACTER, FUNCTION getASBound,LOGICAL, FUNCTION getAppService,CHARACTER, FUNCTION createUiEvents,LOGICAL, FUNCTION getObjectSecured,LOGICAL, FUNCTION getObjectTranslated,LOGICAL, FUNCTION setResizeVertical,LOGICAL,INPUT plResizeVertical LOGICAL FUNCTION setResizeHorizontal,LOGICAL,INPUT plResizeHorizontal LOGICAL FUNCTION setObjectLayout,LOGICAL,INPUT pcLayout CHARACTER FUNCTION setLayoutOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setHideOnInit,LOGICAL,INPUT plHide LOGICAL FUNCTION setDisableOnInit,LOGICAL,INPUT plDisable LOGICAL FUNCTION setDefaultLayout,LOGICAL,INPUT pcDefault CHARACTER FUNCTION setAllFieldNames,LOGICAL,INPUT pcValue CHARACTER FUNCTION setAllFieldHandles,LOGICAL,INPUT pcValue CHARACTER FUNCTION getResizeVertical,LOGICAL, FUNCTION getResizeHorizontal,LOGICAL, FUNCTION getWidth,DECIMAL, FUNCTION getRow,DECIMAL, FUNCTION getObjectLayout,CHARACTER, FUNCTION getObjectEnabled,LOGICAL, FUNCTION getLayoutVariable,CHARACTER, FUNCTION getLayoutOptions,CHARACTER, FUNCTION getHideOnInit,LOGICAL, FUNCTION getHeight,DECIMAL, FUNCTION getEnabledObjHdls,CHARACTER, FUNCTION getEnabledObjFlds,CHARACTER, FUNCTION getDisableOnInit,LOGICAL, FUNCTION getDefaultLayout,CHARACTER, FUNCTION getCol,DECIMAL, FUNCTION getAllFieldNames,CHARACTER, FUNCTION getAllFieldHandles,CHARACTER, FUNCTION setStatusArea,LOGICAL,INPUT plStatusArea LOGICAL FUNCTION getObjectType,character, FUNCTION setWindowTitleViewer,LOGICAL,INPUT phViewer HANDLE FUNCTION setWaitForObject,LOGICAL,INPUT phObject HANDLE FUNCTION setUpdateTarget,LOGICAL,INPUT pcTarget CHARACTER FUNCTION setUpdateSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setTopOnly,LOGICAL,INPUT plTopOnly LOGICAL FUNCTION setSdoForeignFields,LOGICAL,INPUT cSdoForeignFields CHARACTER FUNCTION setSavedContainerMode,LOGICAL,INPUT cSavedContainerMode CHARACTER FUNCTION setRunMultiple,LOGICAL,INPUT plMultiple LOGICAL FUNCTION setRunDOOptions,LOGICAL,INPUT pcOptions CHARACTER FUNCTION setRouterTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setReEnableDataLinks,LOGICAL,INPUT cReEnableDataLinks CHARACTER FUNCTION setPrimarySdoTarget,LOGICAL,INPUT hPrimarySdoTarget HANDLE FUNCTION setPageSource,LOGICAL,INPUT phObject HANDLE FUNCTION setPageNTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setOutMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setNavigationTarget,LOGICAL,INPUT cTarget CHARACTER FUNCTION setNavigationSourceEvents,LOGICAL,INPUT pcEvents CHARACTER FUNCTION setNavigationSource,LOGICAL,INPUT pcSource CHARACTER FUNCTION setMultiInstanceSupported,LOGICAL,INPUT lMultiInstanceSupported LOGICAL FUNCTION setMultiInstanceActivated,LOGICAL,INPUT lMultiInstanceActivated LOGICAL FUNCTION setInMessageTarget,LOGICAL,INPUT phObject HANDLE FUNCTION setFilterSource,LOGICAL,INPUT phObject HANDLE FUNCTION setDynamicSDOProcedure,LOGICAL,INPUT pcProc CHARACTER FUNCTION setDisabledAddModeTabs,LOGICAL,INPUT cDisabledAddModeTabs CHARACTER FUNCTION setCurrentPage,LOGICAL,INPUT iPage INTEGER FUNCTION setContainerTarget,LOGICAL,INPUT pcObject CHARACTER FUNCTION setContainerMode,LOGICAL,INPUT cContainerMode CHARACTER FUNCTION setCallerWindow,LOGICAL,INPUT h HANDLE FUNCTION setCallerProcedure,LOGICAL,INPUT h HANDLE FUNCTION setCallerObject,LOGICAL,INPUT h HANDLE FUNCTION pageNTargets,CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER FUNCTION getStatusArea,LOGICAL, FUNCTION getWindowTitleViewer,HANDLE, FUNCTION getWaitForObject,HANDLE, FUNCTION getUpdateTarget,CHARACTER, FUNCTION getUpdateSource,CHARACTER, FUNCTION getTopOnly,LOGICAL, FUNCTION getSdoForeignFields,CHARACTER, FUNCTION getSavedContainerMode,CHARACTER, FUNCTION getRunMultiple,LOGICAL, FUNCTION getRunDOOptions,CHARACTER, FUNCTION getReEnableDataLinks,CHARACTER, FUNCTION getPrimarySdoTarget,HANDLE, FUNCTION getPageSource,HANDLE, FUNCTION getPageNTarget,CHARACTER, FUNCTION getOutMessageTarget,HANDLE, FUNCTION getNavigationTarget,HANDLE, FUNCTION getNavigationSourceEvents,CHARACTER, FUNCTION getNavigationSource,CHARACTER, FUNCTION getMultiInstanceSupported,LOGICAL, FUNCTION getMultiInstanceActivated,LOGICAL, FUNCTION getFilterSource,HANDLE, FUNCTION getDynamicSDOProcedure,CHARACTER, FUNCTION getDisabledAddModeTabs,CHARACTER, FUNCTION getCurrentPage,INTEGER, FUNCTION getContainerTargetEvents,CHARACTER, FUNCTION getContainerTarget,CHARACTER, FUNCTION getContainerMode,CHARACTER, FUNCTION getCallerWindow,HANDLE, FUNCTION getCallerProcedure,HANDLE, FUNCTION enablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION disablePagesInFolder,LOGICAL,INPUT pcPageInformation CHARACTER FUNCTION widgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION widgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION widgetIsTrue,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsModified,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetIsFocused,LOGICAL,INPUT pcName CHARACTER FUNCTION widgetIsBlank,LOGICAL,INPUT pcNameList CHARACTER FUNCTION widgetLongcharValue,LONGCHAR,INPUT pcName CHARACTER FUNCTION widgetHandle,HANDLE,INPUT pcName CHARACTER FUNCTION viewWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION toggleWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION resetWidgetValue,LOGICAL,INPUT pcNameList CHARACTER FUNCTION highlightWidget,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER FUNCTION hideWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION formattedWidgetValueList,CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION formattedWidgetValue,CHARACTER,INPUT pcName CHARACTER FUNCTION enableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION enableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION disableWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION disableRadioButton,LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER FUNCTION clearWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION blankWidget,LOGICAL,INPUT pcNameList CHARACTER FUNCTION assignWidgetValueList,LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER FUNCTION assignWidgetValue,LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER FUNCTION assignFocusedWidget,LOGICAL,INPUT pcName CHARACTER        �F              44             �� �F  D�              L�              p@    +   �� �  7    � `  8   �� �  H   � �H  I   �% �	  J   �/ (  K   �7 |  L   (9 T  M   |A d  N   �L $  O   N \<  P   `� l  Q   ̋ $  R   � 8  S   (� @  T   h� l	  U   Ը �  V           d� h  ̾ `  ,�    ,� �  ? � �)  iSO8859-1                                                                           E   3 �                                      �                  ��                �E      <  
 K�   <�  @F    �F  ��  �   �F      �F          `                                             PROGRESS                         �           
    
                    �              �                                                                                                     
           �          \  X'  �   P(     �  �B�_�(  
                     H#          $$      �   l         �       �   \  �,  �   �-  �   �  �B�_X.  
                     �(          �)      �   �         �       �   \  �2  �   �3  �   �  �B�_�3  
                     �.          \/      �   T         �       .  B  X'  �   P(     �  �B�_�(  
       .  �         H#          $$      �                          �         �       >  \  ,8  �   $9  >  �      �9  
       >             4          �4      �   $         �       L  \  �=  �   �>  L  �      ,?  
       L             �9          �:      �   �         �       \  L  @A     XA  \  P      �A         \             T?          �?      �   $  	       �       �  L  �D     �D  �  =�      �D         �             �A          4B      �   �  O        
    
                    �             �                                                                                          O          
  L  a      �  
    
                  �  |             8                                                                                          a          
  �  s      t  
    
                  `  (             �                                                                                          s          
  �  �         
    
                    �             �                                                                                          �          
  P  �      �  
    
                  �  �             <                                                                                          �          
  �  �      x  
    
                  d  ,	             �                                                                                          �          
  �	  �      $	  
    
                  	  �	             �	                                                                                          �          
  T
  �      �	  
    
                  �	  �
             @
                                                                                          �          
     �      |
                         h
  0             �
                                                                                          �            �  �      (                          �             �                                                                                          �            X  �      �  
    
                  �  �             D                                                                                          �          
          �  
    
                  l  4             �                                                                                                    
  �        ,  
    
                    �             �                                                                                                    
  \  #      �                        �  �             H                                                                                          #              3      �                        p  8             �                                                                                          3            �  >      0                          �             �                                                                                          >                O      �                        �  �             L                                                                                          O                         INTEGRAL                         PROGRESS                         �  "   "  `      "                         �B�_            "  15                              �  0                      d  @  #      APLIC-IDUSER-IDCODCIAADMINSEGURIDAD                                                   �  #   1"  `      1"                         �B�_            9"  ]                              �                         \  0  )      PROCEDIMIENTODETALLEGRUPOSAPLIC-IDVERSION                                                        %   �"  `      �"                         �B�_            �"  vh                              �                        �  (  s      CODMNUETIQUETATIPOPROGRAMASEGURIDAD-GRUPOSACCESO-DIRECTOICONAPLIC-IDSEGURIDAD-ATRIBUTOSPERSISTENTETECLA-ACELERADORA                                                                       	          
                              �  &   h#  `      h#                         ��            h#  t                              �                              (       �          X  X'  �   P(     �  �B�_�(  
                     H#          $$      �                 �                                             
 � $�          0   �!  �Lp            
             
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
                                                                     �  �  �  �  �  �  �  �      ,  <  L  \  l  |  �  ` � �      ,  <  <L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������		,	<	L	\	l	|	�	�	�	�	�	�	�	�	

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
,<L\l|�   �  �  �  �  �  �  �  �      ,  <  L  \  l  |  �   ` ��      ,  <  <L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������,<L\l|��������			,	<	L	\	l	|	�	�	�	�	�	�	�	�


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
�,<L\l|�                                                                                                                               	                  
                                                  &  &  &  4&  (&                         8&  @&  H&  P&                             T&  \&  h&  p&                              t&  |&  �&  �&                             �&  �&  �&  �&                              �&  �&  �&  �&                              �&  �&  �&  �&                             �&  '  '  '                             '  $'  0'  8'                              <'  D'  L'  T'                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     �(        �(        �(                �     i  i      i  i      i  i     	 	 		 	    0   `   8   @   H   P   X   h   p   x                                                                                                                                  	                  
                                                  �+  �+  �+  �+  �+                         �+  �+  �+  �+                             �+  �+  ,  ,                              ,  ,  ,,  4,                             8,  @,  P,  X,                              \,  d,  l,  t,                              x,  �,  �,  �,                             �,  �,  �,  �,                             �,  �,  �,  �,                              �,  �,  �,  �,                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     �(        �(        �(                �     i  i      i  i      i  i     	 	 		 	    0   `   8   @   H   P   X   h   p   x                                                                                                                                  	                  
                                                  @1  H1  P1  l1  `1                         p1  x1  �1  �1                             �1  �1  �1  �1                              �1  �1  �1  �1                             �1  �1  �1  �1                              �1   2  2  2                              2  2  (2  02                             42  <2  H2  P2                             T2  \2  h2  p2                              t2  |2  �2  �2                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     �(        �(        �(                �     i  i      i  i      i  i     	 	 		 	    0   `   8   @   H   P   X   h   p   x                                                                                                                                  	                  
                                                  �6  �6  �6  7  �6                         7  7  7  $7                             (7  07  <7  D7                              H7  P7  d7  l7                             p7  x7  �7  �7                              �7  �7  �7  �7                              �7  �7  �7  �7                             �7  �7  �7  �7                             �7  �7  8  8                              8  8   8  (8                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     �(        �(        �(                �     i  i      i  i      i  i     	 	 		 	    0   `   8   @   H   P   X   h   p   x                                                                                                                                  	                  
                                                  x<  �<  �<  �<  �<                         �<  �<  �<  �<                             �<  �<  �<  �<                              �<  �<   =  =                             =  =  $=  ,=                              0=  8=  @=  H=                              L=  T=  `=  h=                             l=  t=  �=  �=                             �=  �=  �=  �=                              �=  �=  �=  �=                                                                          Task-No 999999  Numero de Tarea Nro!Tarea   0   Llave-C x(8)    Llave-C     Campo-D 99/99/9999  Campo-D ?   Campo-F ->>>,>>>,>>9.9999   Campo-F 0   Campo-I ->>>,>>>,>>9    Campo-I 0   Campo-C X(8)    Campo-C     Llave-I >>>>>>>>>9  Llave-I 0   Llave-F >>>>>>>>>9  Llave-F 0   Llave-D 99/99/9999  Llave-D ?   Campo-L Si/No   Campo-L Si  �  ���������   � $�  �������������������������������� $�                                �� $�                                �� $�                                � �� B�  �     �(        �(        �(                �     i  i      i  i      i  i     	 	 		 	    0   `   8   @   H   P   X   h   p   x                                                                                               �@  �@  �@  �@  �@                          �@  �@  �@  �@  �@                          �@  �@  �@  A  A                          A  A  $A  <A  ,A                                                                      cmodulo x(15)   cmodulo Cod.Modulo      dmodulo x(80)   dmodulo Nombre del Modulo       cmenu   x(12)   cmenu   Cod.Menu        dmenu   x(80)   dmenu   Opcion del Menu     �  ���������    �      �(                �     i     	    i  q  y                                                                                                                                                      �C  �C  �C  �C  �C                          �C  �C  �C  �C  �C                          �C  �C  �C  D  �C                          D  D  D  4D   D                          8D  @D  HD  \D  PD                          `D  hD  pD  �D  xD                          �D  �D  �D  �D  �D                                                                      cuser   x(15)   cuser   Cod.Usuario     duser   x(15)   duser   Nombres     cmodulo x(15)   cmodulo Cod.Modulo      dmodulo x(80)   dmodulo Nombre del Modulo       cmenu   x(12)   cmenu   Cod.Menu        dmenu   x(80)   dmenu   Opcion del Menu     dtipo   x(80)   dtipo   Tipo        �  ���	������       �       �(                �     i    	 	    �  �  i  q  y    �    ��                                                                                                                                                      ����                            �    ��  2                 S�    9   ��  2                 S�    ^   ��  2                 S�    �(         �)  " �4    �)  # �8    �)  % �    �(         �)  &       �(         �(         getOtherWinTargetFrame  undefined                                                               �        �  �   l   �    ��                  �����               �ڲ                    O   ����    e�          O   ����    R�          O   ����    ��      t       �   �              4   ����     /                                    3   ����       $     H  ���                       8      
                       � ߱        �  �      D            C          assignFocusedWidget         �      �     �       LOGICAL,INPUT pcName CHARACTER  assignWidgetValue   �      �      $    �       LOGICAL,INPUT pcName CHARACTER,INPUT pcValue CHARACTER  assignWidgetValueList         \      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT pcValueList CHARACTER,INPUT pcDelimiter CHARACTER  blankWidget t      �          �       LOGICAL,INPUT pcNameList CHARACTER  clearWidget �      @      l    �       LOGICAL,INPUT pcNameList CHARACTER  disableRadioButton  L      �      �    �       LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    disableWidget   �            4           LOGICAL,INPUT pcNameList CHARACTER  enableRadioButton         X      �          LOGICAL,INPUT pcNameList CHARACTER,INPUT piButtonNum INTEGER    enableWidget    l      �      �           LOGICAL,INPUT pcNameList CHARACTER  formattedWidgetValue    �             X  	  -      CHARACTER,INPUT pcName CHARACTER    formattedWidgetValueList    8      |      �  
  B      CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER    hideWidget  �      �      (   
 [      LOGICAL,INPUT pcNameList CHARACTER  highlightWidget       L      |    f      LOGICAL,INPUT pcNameList CHARACTER,INPUT pcHighlightType CHARACTER  resetWidgetValue    \      �      �    v      LOGICAL,INPUT pcNameList CHARACTER  toggleWidget    �            H    �      LOGICAL,INPUT pcNameList CHARACTER  viewWidget  (      l      �   
 �      LOGICAL,INPUT pcNameList CHARACTER  widgetHandle    x      �      �    �      HANDLE,INPUT pcName CHARACTER   widgetLongcharValue �            @    �      LONGCHAR,INPUT pcName CHARACTER widgetIsBlank          `      �    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsFocused p      �      �    �      LOGICAL,INPUT pcName CHARACTER  widgetIsModified    �      	      8	    �      LOGICAL,INPUT pcNameList CHARACTER  widgetIsTrue    	      \	      �	    �      LOGICAL,INPUT pcName CHARACTER  widgetValue l	      �	      �	    �      CHARACTER,INPUT pcName CHARACTER    widgetValueList �	      �	      ,
          CHARACTER,INPUT pcNameList CHARACTER,INPUT pcDelimiter CHARACTER        u   ����  �             d   �           �   �          �   �          �   �              � ߱            Z   �����
   �p
                         u   ���� �             �   �             �          <  �              � ߱            Z   ����$   �                         u   ���� �             `  �           �  �          �  �              � ߱            Z   �����   ��                     P    �  4  D  �  �      4   �����      o   �  
     x                              �  �  NA    �    �  (     <     P    d    x    �    �    �  `  �  
`  �  $  �               $  �  $  ���                       ,     
 
                   � ߱        ��      l  �      4      4   ����4                �                      ��                                      ,ɳ                         |  |        $      h      4   ����h      $    P  ���                       �  @         �              � ߱                �  �             4   ����       $    �  ���                       P  @         <              � ߱        assignPageProperty                              �  �      ��                  �  �  �              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            changePage                              �  �      ��                  �  �                 ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            confirmExit                             �  �      ��                  �  �                 �ѳ                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            constructObject                               �      ��                  �  �  ,              �ų                    O   ����    e�          O   ����    R�          O   ����    ��            ��   x             D               �� 
  �             l  
             ��   �             �               �� 
                 �  
         ��                            ����                            createObjects                               �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            deletePage                              �  �      ��                  �  �  �              D��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �           ��                            ����                            destroyObject                               �  �      ��                  �  �  �              �޳                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hidePage                                �  �      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                             ��                            ����                            initializeObject                                  �      ��                  �  �  ,              T:�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeVisualContainer                               $        ��                  �  �  <              ;�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initPages                               $        ��                  �  �  <              (2�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  T           ��                            ����                            notifyPage                              L  4      ��                  �  �  d              �                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  |           ��                            ����                            passThrough                             t  \      ��                  �  �  �              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �             �               ��                  �           ��                            ����                            removePageNTarget                               �  �      ��                  �  �  �              �+�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  0             �  
             ��                  $           ��                            ����                            selectPage                                        ��                  �  �  4               ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  L            ��                            ����                            toolbar                             @!  (!      ��                  �  �  X!              ,-�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  p!           ��                            ����                            viewObject                              h"  P"      ��                  �  �  �"              h��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            viewPage                                h#  P#      ��                  �  �  �#              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �#           ��                            ����                            disablePagesInFolder    
       $      8$    �      LOGICAL,INPUT pcPageInformation CHARACTER   enablePagesInFolder $      d$      �$    �      LOGICAL,INPUT pcPageInformation CHARACTER   getCallerProcedure  x$      �$      �$    �      HANDLE, getCallerWindow �$       %      0%    �      HANDLE, getContainerMode    %      8%      l%    �      CHARACTER,  getContainerTarget  L%      x%      �%          CHARACTER,  getContainerTargetEvents    �%      �%      �%          CHARACTER,  getCurrentPage  �%       &      0&    2      INTEGER,    getDisabledAddModeTabs  &      <&      t&     A      CHARACTER,  getDynamicSDOProcedure  T&      �&      �&  !  X      CHARACTER,  getFilterSource �&      �&      �&  "  o      HANDLE, getMultiInstanceActivated   �&      �&      8'  #        LOGICAL,    getMultiInstanceSupported   '      D'      �'  $  �      LOGICAL,    getNavigationSource `'      �'      �'  %  �      CHARACTER,  getNavigationSourceEvents   �'      �'      (  &  �      CHARACTER,  getNavigationTarget �'      (      H(  '  �      HANDLE, getOutMessageTarget ((      P(      �(  (  �      HANDLE, getPageNTarget  d(      �(      �(  )  	      CHARACTER,  getPageSource   �(      �(      �(  *        HANDLE, getPrimarySdoTarget �(       )      4)  +  &      HANDLE, getReEnableDataLinks    )      <)      t)  ,  :      CHARACTER,  getRunDOOptions T)      �)      �)  -  O      CHARACTER,  getRunMultiple  �)      �)      �)  .  _      LOGICAL,    getSavedContainerMode   �)      �)      0*  /  n      CHARACTER,  getSdoForeignFields *      <*      p*  0  �      CHARACTER,  getTopOnly  P*      |*      �*  1 
 �      LOGICAL,    getUpdateSource �*      �*      �*  2  �      CHARACTER,  getUpdateTarget �*      �*       +  3  �      CHARACTER,  getWaitForObject     +      ,+      `+  4  �      HANDLE, getWindowTitleViewer    @+      h+      �+  5  �      HANDLE, getStatusArea   �+      �+      �+  6  �      LOGICAL,    pageNTargets    �+      �+      ,  7  �      CHARACTER,INPUT phTarget HANDLE,INPUT piPageNum INTEGER setCallerObject �+      L,      |,  8        LOGICAL,INPUT h HANDLE  setCallerProcedure  \,      �,      �,  9        LOGICAL,INPUT h HANDLE  setCallerWindow �,      �,      -  :  '      LOGICAL,INPUT h HANDLE  setContainerMode    �,      (-      \-  ;  7      LOGICAL,INPUT cContainerMode CHARACTER  setContainerTarget  <-      �-      �-  <  H      LOGICAL,INPUT pcObject CHARACTER    setCurrentPage  �-      �-      .  =  [      LOGICAL,INPUT iPage INTEGER setDisabledAddModeTabs  �-      (.      `.  >  j      LOGICAL,INPUT cDisabledAddModeTabs CHARACTER    setDynamicSDOProcedure  @.      �.      �.  ?  �      LOGICAL,INPUT pcProc CHARACTER  setFilterSource �.      �.      /  @  �      LOGICAL,INPUT phObject HANDLE   setInMessageTarget  �.      8/      l/  A  �      LOGICAL,INPUT phObject HANDLE   setMultiInstanceActivated   L/      �/      �/  B  �      LOGICAL,INPUT lMultiInstanceActivated LOGICAL   setMultiInstanceSupported   �/      �/      40  C  �      LOGICAL,INPUT lMultiInstanceSupported LOGICAL   setNavigationSource 0      d0      �0  D  �      LOGICAL,INPUT pcSource CHARACTER    setNavigationSourceEvents   x0      �0      �0  E        LOGICAL,INPUT pcEvents CHARACTER    setNavigationTarget �0      1      P1  F        LOGICAL,INPUT cTarget CHARACTER setOutMessageTarget 01      p1      �1  G  1      LOGICAL,INPUT phObject HANDLE   setPageNTarget  �1      �1      �1  H  E      LOGICAL,INPUT pcObject CHARACTER    setPageSource   �1      2      H2  I  T      LOGICAL,INPUT phObject HANDLE   setPrimarySdoTarget (2      h2      �2  J  b      LOGICAL,INPUT hPrimarySdoTarget HANDLE  setReEnableDataLinks    |2      �2      �2  K  v      LOGICAL,INPUT cReEnableDataLinks CHARACTER  setRouterTarget �2      (3      X3  L  �      LOGICAL,INPUT phObject HANDLE   setRunDOOptions 83      x3      �3  M  �      LOGICAL,INPUT pcOptions CHARACTER   setRunMultiple  �3      �3      �3  N  �      LOGICAL,INPUT plMultiple LOGICAL    setSavedContainerMode   �3       4      X4  O  �      LOGICAL,INPUT cSavedContainerMode CHARACTER setSdoForeignFields 84      �4      �4  P  �      LOGICAL,INPUT cSdoForeignFields CHARACTER   setTopOnly  �4      �4      5  Q 
 �      LOGICAL,INPUT plTopOnly LOGICAL setUpdateSource �4      05      `5  R  �      LOGICAL,INPUT pcSource CHARACTER    setUpdateTarget @5      �5      �5  S  �      LOGICAL,INPUT pcTarget CHARACTER    setWaitForObject    �5      �5      6  T        LOGICAL,INPUT phObject HANDLE   setWindowTitleViewer    �5      ,6      d6  U         LOGICAL,INPUT phViewer HANDLE   getObjectType   D6      �6      �6  V  5      CHARACTER,  setStatusArea   �6      �6      �6  W  C      LOGICAL,INPUT plStatusArea LOGICAL  applyLayout                             �7  �7      ��                  ]  ^  �7              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disableObject                               �8  �8      ��                  `  a  �8              x��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            enableObject                                �9  �9      ��                  c  d  �9              00�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �:  �:      ��                  f  g  �:              �0�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            processAction                               �;  �;      ��                  i  k  �;              ���                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �;           ��                            ����                            getAllFieldHandles  �6      P<      �<  X  Q      CHARACTER,  getAllFieldNames    d<      �<      �<  Y  d      CHARACTER,  getCol  �<      �<      �<  Z  u      DECIMAL,    getDefaultLayout    �<      =      8=  [  |      CHARACTER,  getDisableOnInit    =      D=      x=  \  �      LOGICAL,    getEnabledObjFlds   X=      �=      �=  ]  �      CHARACTER,  getEnabledObjHdls   �=      �=      �=  ^  �      CHARACTER,  getHeight   �=      >      0>  _ 	 �      DECIMAL,    getHideOnInit   >      <>      l>  `  �      LOGICAL,    getLayoutOptions    L>      x>      �>  a  �      CHARACTER,  getLayoutVariable   �>      �>      �>  b  �      CHARACTER,  getObjectEnabled    �>      �>      ,?  c  �      LOGICAL,    getObjectLayout ?      8?      h?  d  	      CHARACTER,  getRow  H?      t?      �?  e  	      DECIMAL,    getWidth    |?      �?      �?  f  %	      DECIMAL,    getResizeHorizontal �?      �?      @  g  .	      LOGICAL,    getResizeVertical   �?       @      T@  h  B	      LOGICAL,    setAllFieldHandles  4@      `@      �@  i  T	      LOGICAL,INPUT pcValue CHARACTER setAllFieldNames    t@      �@      �@  j  g	      LOGICAL,INPUT pcValue CHARACTER setDefaultLayout    �@      A      <A  k  x	      LOGICAL,INPUT pcDefault CHARACTER   setDisableOnInit    A      `A      �A  l  �	      LOGICAL,INPUT plDisable LOGICAL setHideOnInit   tA      �A      �A  m  �	      LOGICAL,INPUT plHide LOGICAL    setLayoutOptions    �A      B      8B  n  �	      LOGICAL,INPUT pcOptions CHARACTER   setObjectLayout B      \B      �B  o  �	      LOGICAL,INPUT pcLayout CHARACTER    setResizeHorizontal lB      �B      �B  p  �	      LOGICAL,INPUT plResizeHorizontal LOGICAL    setResizeVertical   �B      C      DC  q  �	      LOGICAL,INPUT plResizeVertical LOGICAL  getObjectTranslated $C      lC      �C  r  �	      LOGICAL,    getObjectSecured    �C      �C      �C  s  
      LOGICAL,    createUiEvents  �C      �C      D  t  
      LOGICAL,    bindServer                              �D  �D      ��                  M  N  �D              �Ұ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �E  �E      ��                  P  Q  �E              4Ӱ                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyServerObject                             �F  �F      ��                  S  T  �F              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            disconnectObject                                �G  �G      ��                  V  W  �G              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeServerObject                              �H  �H      ��                  Y  Z  �H              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            restartServerObject                             �I  �I      ��                  \  ]  �I              h�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            runServerObject                             �J  �J      ��                  _  a  �J              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 K  
         ��                            ����                            startServerObject                               L  �K      ��                  c  d  ,L              �d�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            unbindServer                                M   M      ��                  f  h  0M              Pe�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  HM           ��                            ����                            getAppService   �C      �M      �M  u  #
      CHARACTER,  getASBound  �M      �M      N  v 
 1
      LOGICAL,    getAsDivision   �M      $N      TN  w  <
      CHARACTER,  getASHandle 4N      `N      �N  x  J
      HANDLE, getASHasStarted lN      �N      �N  y  V
      LOGICAL,    getASInfo   �N      �N      �N  z 	 f
      CHARACTER,  getASInitializeOnRun    �N      O      @O  {  p
      LOGICAL,    getASUsePrompt   O      LO      |O  |  �
      LOGICAL,    getServerFileName   \O      �O      �O  }  �
      CHARACTER,  getServerOperatingMode  �O      �O       P  ~  �
      CHARACTER,  runServerProcedure  �O      P      @P    �
      HANDLE,INPUT pcServerFileName CHARACTER,INPUT phAppService HANDLE   setAppService    P      �P      �P  �  �
      LOGICAL,INPUT pcAppService CHARACTER    setASDivision   �P      �P      Q  �  �
      LOGICAL,INPUT pcDivision CHARACTER  setASHandle �P      0Q      \Q  �  �
      LOGICAL,INPUT phASHandle HANDLE setASInfo   <Q      |Q      �Q  � 	 �
      LOGICAL,INPUT pcInfo CHARACTER  setASInitializeOnRun    �Q      �Q       R  �        LOGICAL,INPUT plInitialize LOGICAL  setASUsePrompt  �Q      $R      TR  �        LOGICAL,INPUT plFlag LOGICAL    setServerFileName   4R      tR      �R  �  &      LOGICAL,INPUT pcFileName CHARACTER  setServerOperatingMode  �R      �R      S  �  8      LOGICAL,INPUT pcServerOperatingMode CHARACTER   addLink                             �S  �S      ��                  +  /  �S              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  $T             �S  
             ��   LT             T               �� 
                 @T  
         ��                            ����                            addMessage                              8U   U      ��                  1  5  PU              4��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �U             hU               ��   �U             �U               ��                  �U           ��                            ����                            adjustTabOrder                              �V  �V      ��                  7  ;  �V              9�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  W             �V  
             �� 
  @W             W  
             ��                  4W           ��                            ����                            applyEntry                              ,X  X      ��                  =  ?  DX              �.�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  \X           ��                            ����                            changeCursor                                XY  @Y      ��                  A  C  pY              /�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  �Y           ��                            ����                            createControls                              �Z  lZ      ��                  E  F  �Z              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            destroyObject                               �[  p[      ��                  H  I  �[               �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            displayLinks                                �\  t\      ��                  K  L  �\              �                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            editInstanceProperties                              �]  �]      ��                  N  O  �]              P��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            exitObject                              �^  �^      ��                  Q  R  �^              ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            hideObject                              �_  �_      ��                  T  U  �_              �±                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            initializeObject                                �`  �`      ��                  W  X  �`              L6�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            modifyListProperty                              �a  �a      ��                  Z  _  �a              l7�                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  b             �a  
             ��   4b              b               ��   \b             (b               ��                  Pb           ��                            ����                            modifyUserLinks                             Lc  4c      ��                  a  e  dc              `Ʊ                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �c             |c               ��   �c             �c               �� 
                 �c  
         ��                            ����                            removeAllLinks                              �d  �d      ��                  g  h  �d              �α                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            removeLink                              �e  �e      ��                  j  n  �e              �                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
  ,f             �e  
             ��   Tf              f               �� 
                 Hf  
         ��                            ����                            repositionObject                                Hg  0g      ��                  p  s  `g              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   �g             xg               ��                  �g           ��                            ����                            returnFocus                             �h  �h      ��                  u  w  �h              ���                    O   ����    e�          O   ����    R�          O   ����    ��            �� 
                 �h  
         ��                            ����                            showMessageProcedure                                �i  �i      ��                  y  |  �i              ��                    O   ����    e�          O   ����    R�          O   ����    ��            ��   0j             �i               ��                  $j           ��                            ����                            toggleData                              k  k      ��                  ~  �  4k              \�                    O   ����    e�          O   ����    R�          O   ����    ��            ��                  Lk           ��                            ����                            viewObject                              Dl  ,l      ��                  �  �  \l              ��                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                            anyMessage  �R      �l      �l  � 
 �      LOGICAL,    assignLinkProperty  �l      �l       m  �  �      LOGICAL,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER   fetchMessages    m      xm      �m  �  �      CHARACTER,  getChildDataKey �m      �m      �m  �  �      CHARACTER,  getContainerHandle  �m      �m      $n  �  �      HANDLE, getContainerHidden  n      ,n      `n  �  �      LOGICAL,    getContainerSource  @n      ln      �n  �  �      HANDLE, getContainerSourceEvents    �n      �n      �n  �        CHARACTER,  getContainerType    �n      �n      $o  �  +      CHARACTER,  getDataLinksEnabled o      0o      do  �  <      LOGICAL,    getDataSource   Do      po      �o  �  P      HANDLE, getDataSourceEvents �o      �o      �o  �  ^      CHARACTER,  getDataSourceNames  �o      �o      p  �  r      CHARACTER,  getDataTarget   �o      (p      Xp  �  �      CHARACTER,  getDataTargetEvents 8p      dp      �p  �  �      CHARACTER,  getDBAware  xp      �p      �p  � 
 �      LOGICAL,    getDesignDataObject �p      �p      q  �  �      CHARACTER,  getDynamicObject    �p      q      Pq  �  �      LOGICAL,    getInstanceProperties   0q      \q      �q  �  �      CHARACTER,  getLogicalObjectName    tq      �q      �q  �  �      CHARACTER,  getLogicalVersion   �q      �q      r  �        CHARACTER,  getObjectHidden �q      $r      Tr  �        LOGICAL,    getObjectInitialized    4r      `r      �r  �  $      LOGICAL,    getObjectName   xr      �r      �r  �  9      CHARACTER,  getObjectPage   �r      �r      s  �  G      INTEGER,    getObjectParent �r      s      Ls  �  U      HANDLE, getObjectVersion    ,s      Ts      �s  �  e      CHARACTER,  getObjectVersionNumber  hs      �s      �s  �  v      CHARACTER,  getParentDataKey    �s      �s      t  �  �      CHARACTER,  getPassThroughLinks �s      t      Lt  �  �      CHARACTER,  getPhysicalObjectName   ,t      Xt      �t  �  �      CHARACTER,  getPhysicalVersion  pt      �t      �t  �  �      CHARACTER,  getPropertyDialog   �t      �t      u  �  �      CHARACTER,  getQueryObject  �t      u      Lu  �  �      LOGICAL,    getRunAttribute ,u      Xu      �u  �  �      CHARACTER,  getSupportedLinks   hu      �u      �u  �        CHARACTER,  getTranslatableProperties   �u      �u      v  �        CHARACTER,  getUIBMode  �u      v      Hv  � 
 8      CHARACTER,  getUserProperty (v      Tv      �v  �  C      CHARACTER,INPUT pcPropName CHARACTER    instancePropertyList    dv      �v      �v  �  S      CHARACTER,INPUT pcPropList CHARACTER    linkHandles �v      w      8w  �  h      CHARACTER,INPUT pcLink CHARACTER    linkProperty    w      \w      �w  �  t      CHARACTER,INPUT pcLink CHARACTER,INPUT pcPropName CHARACTER mappedEntry lw      �w      �w  �  �      CHARACTER,INPUT pcEntry CHARACTER,INPUT pcList CHARACTER,INPUT plFirst LOGICAL,INPUT pcDelimiter CHARACTER  messageNumber   �w      `x      �x  �  �      CHARACTER,INPUT piMessage INTEGER   propertyType    px      �x      �x  �  �      CHARACTER,INPUT pcPropName CHARACTER    reviewMessages  �x      y      <y  �  �      CHARACTER,  setChildDataKey y      Hy      xy  �  �      LOGICAL,INPUT cChildDataKey CHARACTER   setContainerHidden  Xy      �y      �y  �  �      LOGICAL,INPUT plHidden LOGICAL  setContainerSource  �y      �y      (z  �  �      LOGICAL,INPUT phObject HANDLE   setContainerSourceEvents    z      Hz      �z  �  �      LOGICAL,INPUT pcEvents CHARACTER    setDataLinksEnabled dz      �z      �z  �        LOGICAL,INPUT lDataLinksEnabled LOGICAL setDataSource   �z      {      4{  �        LOGICAL,INPUT phObject HANDLE   setDataSourceEvents {      T{      �{  �  (      LOGICAL,INPUT pcEventsList CHARACTER    setDataSourceNames  h{      �{      �{  �  <      LOGICAL,INPUT pcSourceNames CHARACTER   setDataTarget   �{      |      <|  �  O      LOGICAL,INPUT pcTarget CHARACTER    setDataTargetEvents |      `|      �|  �  ]      LOGICAL,INPUT pcEvents CHARACTER    setDBAware  t|      �|      �|  � 
 q      LOGICAL,INPUT lAware LOGICAL    setDesignDataObject �|      }      8}  �  |      LOGICAL,INPUT pcDataObject CHARACTER    setDynamicObject    }      `}      �}  �  �      LOGICAL,INPUT lTemp LOGICAL setInstanceProperties   t}      �}      �}  �  �      LOGICAL,INPUT pcPropList CHARACTER  setLogicalObjectName    �}      ~      D~  �  �      LOGICAL,INPUT c CHARACTER   setLogicalVersion   $~      `~      �~  �  �      LOGICAL,INPUT cVersion CHARACTER    setObjectName   t~      �~      �~  �  �      LOGICAL,INPUT pcName CHARACTER  setObjectParent �~            8  �  �      LOGICAL,INPUT phParent HANDLE   setObjectVersion          X      �  �  �      LOGICAL,INPUT cObjectVersion CHARACTER  setParentDataKey    l      �      �  �        LOGICAL,INPUT cParentDataKey CHARACTER  setPassThroughLinks �      �      D�  �        LOGICAL,INPUT pcLinks CHARACTER setPhysicalObjectName   $�      d�      ��  �  2      LOGICAL,INPUT cTemp CHARACTER   setPhysicalVersion  |�      ��      ��  �  H      LOGICAL,INPUT cVersion CHARACTER    setRunAttribute Ѐ      �      D�  �  [      LOGICAL,INPUT cRunAttribute CHARACTER   setSupportedLinks   $�      l�      ��  �  k      LOGICAL,INPUT pcLinkList CHARACTER  setTranslatableProperties   ��      ā       �  �  }      LOGICAL,INPUT pcPropList CHARACTER  setUIBMode  ��      $�      P�  � 
 �      LOGICAL,INPUT pcMode CHARACTER  setUserProperty 0�      p�      ��  �  �      LOGICAL,INPUT pcPropName CHARACTER,INPUT pcPropValue CHARACTER  showmessage ��      ��      �  �  �      LOGICAL,INPUT pcMessage CHARACTER   Signature   �      0�      \�  � 	 �      CHARACTER,INPUT pcName CHARACTER    T�    �  ��  �      �      4   �����                (�                      ��                  �  �                  䀲                       �  ��        �  D�  ��      �      4   �����                Є                      ��                  �  �                  X��                       �  T�  Ѕ    �  �  h�      �      4   �����                x�                      ��                  �  �                  ܁�                       �  ��         �                                  x     
 
                   � ߱        ��  $  �  ��  ���                           $  �  (�  ���                       �      
 	       	           � ߱        `�    �  p�  �      �      4   �����                ��                      ��                  �  �	                  ��                       �  ��  0�  o   �  
    ,                                 ��  $   �  \�  ���                       H  @         4              � ߱        ��  �   �  h      ��  �   �  �      ć  �   �  P      ؇  �   �  �      �  �   �  8       �  �   �  �      �  �   �  (	      (�  �   �  d	      <�  �   �  �	      P�  �   �  L
      d�  �   �  �
      x�  �   �  D      ��  �   �  �      ��  �   �  �      ��  �   �  x      Ȉ  �   �  �      ܈  �   �  (      ��  �   �  �      �  �   �  �      �  �   �  L      ,�  �   �  �      @�  �   �  <      T�  �   	  �      h�  �   	  ,      |�  �   	  �      ��  �   	        ��  �   	  �      ��  �   	  �      ̉  �   
	  @      ��  �   	  |      �  �   	  �      �  �   	  ,      �  �   	  h      0�  �   	  �      D�  �   	  �      X�  �   	  \      l�  �   	  �      ��  �   	  �      ��  �   	        ��  �   	  L      ��  �   	  �      Њ  �   	  �      �  �   	         ��  �   	  <          �   	  x                      $�          ��  x�      ��                  �	  �	  ��              Ї�                    O   ����    e�          O   ����    R�          O   ����    ��      �     
 
               d      
 
       
       t                         � ߱        P�  $ �	  ��  ���                           O   �	  ��  ��  �               ��          ��  ��    ��                                             ��                            ����                                �6      �      h�     6     Č                      V ��  5                      �    
  |�  ��      �      4   �����                �                      ��                  
  �
                  �P�                       
  ��  �  �   	
         0�  �   

  �      D�  �   
        X�  �   
  �      l�  �   
        ��  �   
  �      ��  �   
  �      ��  �   
  t      ��  �   
  �      Ў  �   
  l      �  �   
  �      ��  �   
  \      �  �   
  �          �   
  T      ��    �
  <�  ��      �      4   �����                ȏ                      ��                  �
  &                  �R�                       �
  L�  ܏  �   �
  $       ��  �   �
  �       �  �   �
  !      �  �   �
  �!      ,�  �   �
  �!      @�  �   �
  p"      T�  �   �
  �"      h�  �   �
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
  �,      �    2  �  ��      X-      4   ����X-                ��                      ��                  3  �                  �m�                       3  $�  ��  �   6  �-      Ȓ  �   7  4.      ܒ  �   8  �.      �  �   9  $/      �  �   ;  �/      �  �   <  0      ,�  �   >  �0      @�  �   ?  �0      T�  �   @  01      h�  �   A  l1      |�  �   B  �1      ��  �   C  2      ��  �   D  �2      ��  �   E  3      ̓  �   G  �3      ��  �   H  �3      ��  �   I  h4      �  �   J  �4      �  �   K  `5      0�  �   L  �5      D�  �   N  6      X�  �   O  �6      l�  �   P  �6      ��  �   Q  47      ��  �   R  p7      ��  �   S  �7      ��  �   T  (8      Д  �   U  d8      �  �   V  �8      ��  �   W  �8      �  �   X  9       �  �   Y  T9      4�  �   Z  �9      H�  �   \  :      \�  �   ]  @:      p�  �   ^  |:      ��  �   _  �:      ��  �   `  �:      ��  �   a  0;      ��  �   b  l;      ԕ  �   c  �;      �  �   d  <      ��  �   e  �<      �  �   f  =      $�  �   g  x=      8�  �   h  �=      L�  �   i  p>      `�  �   j  �>      t�  �   k  h?      ��  �   l  �?      ��  �   m  `@      ��  �   n  �@      Ė  �   o  A      ؖ  �   p  TA      �  �   q  �A       �  �   r  �A          �   s  @B      l�  $  �  @�  ���                       �B     
 
                   � ߱        �    )  ��  ��      �B      4   �����B      /   *  ė     ԗ                          3   �����B            ��                      3   �����B  X�    3   �  ��  ��   C      4   ���� C  	              ��                      ��             	     4  �                  ��                       4  0�  ��  �   8  `C      �  $  9  �  ���                       �C     
 
                   � ߱        ,�  �   :  �C      ��  $   <  X�  ���                       �C  @         �C              � ߱        @�  $  ?  ��  ���                       (D      
                   � ߱        �D     
 
               E      
 
       
       hF  @        
 (F              � ߱        К  V   I  ܙ  ���                        tF      
               �F      
               �F      
                   � ߱        `�  $  e  l�  ���                       �G     
 
                H      
 
       
       pI  @        
 0I              � ߱        �  V   w  ��  ���                        |I     
 
               �I      
 
       
       HK  @        
 K              � ߱            V   �  ��  ���                        
              P�                      ��             
     �  W                  ���                       �  �  TK     
 
               �K      
 
       
        M  @        
 �L          �M  @        
 DM          �M  @        
 �M          DN  @        
 N              � ߱            V   �  ��  ���                        adm-clone-props �  |�              �     7     `                          \                       start-super-proc    ��  �  �           �     8                                  )                     �    o  t�  ��      �Q      4   �����Q      /   p  ��     ��                          3   �����Q            ��                      3   ���� R  H�  $  �  �  ���                        R      
                   � ߱        �    �  d�  ��  ��  <R      4   ����<R                T�                      ��                  �  �                  �.�                       �  t�  PR      
               dR      
               xR      
                   � ߱            $  �  �  ���                             �  ��  ؠ      �R      4   �����R  �R      
                   � ߱            $  �  ��  ���                        �    �   �  0�  ��  �R      4   �����R      $  �  \�  ���                       �R      
                   � ߱            �   �  �R      8S     
 
               �S      
 
       
       U  @        
 �T              � ߱        ,�  V   �  ��  ���                        @�  �     U      آ    �  \�  l�      PU      4   ����PU      /   �  ��     ��                          3   ����`U            Ȣ                      3   �����U  ��  $  �  �  ���                       �U      
                   � ߱        �U     
 
               DV      
 
       
       �W  @        
 TW              � ߱        ��  V   �  0�  ���                        ��      ܣ  X�      �W      4   �����W                h�                      ��                                      �S�                         �      g     ��         �D�                           H�          �   �      ��                        0�              (T�                    O   ����    e�          O   ����    R�          O   ����    ��          /    t�     ��  �W                      3   �����W  ��     
   ��                      3   �����W         
   ԥ                      3   �����W    ��                              ��                          ����                                        ��              9      �                      g                               ��  g     ��          �	L�                           ��          P�  8�      ��                    !  h�              W�                    O   ����    e�          O   ����    R�          O   ����    ��          /     ��     ��   X                      3   �����W            ܧ                      3   ����X    ��                              ��                          ����                                        ̦              :      �                      g                               ��  g   #  ��          �	T�                           ��          X�  @�      ��                  #  %  p�              �W�                    O   ����    e�          O   ����    R�          O   ����    ��          /  $  ��     ĩ  @X                      3   ����$X            �                      3   ����HX    ��                              ��                          ����                                        Ԩ              ;      ��                      g                               �    <  ̪  H�      dX      4   ����dX                X�                      ��                  =  \                  XX�                       =  ܪ  ī  /   >  ��     ��                          3   ����tX            ��                      3   �����X  ��  /  @  �      �  �X                      3   �����X  0�     
    �                      3   �����X  `�        P�                      3   �����X  ��        ��                      3   �����X            ��                      3   ����Y  �    H  ܬ  �      <Y      4   ����<Y      /  N  �     (�  �Y                      3   �����Y  X�     
   H�                      3   �����Y  ��        x�                      3   �����Y  ��        ��                      3   �����Y            ح                      3   ����Z        T  �  �      ,Z      4   ����,Z      /  W  @�     P�  �Z                      3   ����`Z  ��     
   p�                      3   �����Z  ��        ��                      3   �����Z  �        Ю                      3   �����Z             �                      3   �����Z  б    `  ,�  ��      �Z      4   �����Z                ��                      ��                  a  d                  ,L�                       a  <�      g   b  Я         �t�        �Z                  ��          h�  P�      ��                  c      ��              �B�                    O   ����    e�          O   ����    R�          O   ����    ��          /  c  İ     ԰  [                      3   ���� [  �     
   ��                      3   ����$[         
   $�                      3   ����,[    ��                            ����                                        �              <      4�                      g                               h�     h  4[                                     H[     
 
               �[      
 
       
       ]  @        
 �\              � ߱        ��  V   �  �  ���                        (]     
 
               �]      
 
       
       �^  @        
 �^              � ߱        $�  V   �  ��  ���                        ��    /  @�  P�      _      4   ����_      $   0  |�  ���                       h_  @         T_              � ߱        |�  g   p  ��         � �        |_  � �        �_                  ��          l�  T�      ��                  q  v  ��              Ho�                    O   ����    e�          O   ����    R�          O   ����    ��            u  ��  ȴ      �_      4   �����_      O  u  ������  �_    ��                            ����                                        �              =      �                      g                               (�  g   }  ��         6̶         �_                  \�          ,�  �      ��                  ~  �  D�              �o�                    O   ����    e�          O   ����    R�          O   ����    ��      t�    �  �_  }          O  �  ������  �_    ��                            ����                                        ��              >      ��                      g                               ĸ  g   �  @�         !h�                                        ط  ��      ��                  �  �  �              Hp�                    O   ����    e�          O   ����    R�          O   ����    ��        ��                              ��                          ����                                        T�              ?      �                      g                               غ  g   �  ܸ         4|�                            ��          t�  \�      ��                  �  �  ��              Ps�                    O   ����    e�          O   ����    R�          O   ����    ��      ��  �   �  �_      ̹  �   �  `      �  /   �  ��                                 3   ����0`      �   �  L`        ��                              ��                          ����                                        �              @      �                      g                               ��  g   �  �         "��                           ��          ��  p�      ��                 �  �  ��              �s�                    O   ����    e�          O   ����    R�          O   ����    ��      4�  �   �           D�      ؽ  ��      ��  ��      ��                  �  �  ��              @3�                ��     �  Ȼ      p�  ��       ��                            7   ����          ��                     �            �                  6   �        4�   ��                    �            �                                                                |�  p�                                   @            P�   `�        O   ����  e�          O   ����  R�          O   ����  ��      h�  9   �     l`                     �`                     �`                     �`                         � ߱            $  �  �  ���                       о  /  �  ��                               3   �����`  (�  $  �  ��  ���                       a                         � ߱        L�  /  �  T�     d�  �a                      3   ����ha  ��     
   ��                      3   �����a  Ŀ        ��                      3   �����a            �  ��                  3   �����a      $   �   �  ���                                                   � ߱        p�  /  �  x�     ��  �a                      3   �����a  ��     
   ��                      3   �����a  ��        ��                      3   ���� b            �  �                  3   ����b      $   �  D�  ���                                                   � ߱            �   �  b               �          ��  ��   @ ��            
                                                 0              0   �       ��                              ��                           ��                            ����                                =   �     ��          �      ��     A     �                      g   �                          ��  g   �  �         "t�                           ��          ��  ��      ��                  �  �  ��              h�                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  �                                 3   ����$b    ��                              ��                          ����                                        $�              B      �                      g                               T�  g   �  ��         4��                           ��          ��  h�      ��                  �  �  ��              ��                    O   ����    e�          O   ����    R�          O   ����    ��             
                   � ߱        �  $   �  ��   �                       `�  $  �  4�  ���                       Pb                         � ߱            s   �  ��       \�                      ��  �  ��                               7   ����          ����                �b   �            X�                  6   �         |�  ����               �b   �            X�                                                                ��  ��                                   @            ��   ��        J   �        ��@�    ��                                                          �b  �b  c                      (�                 \b   hb   �b    ��                              ��                          ����                            �        2                 S�                ��              C      t�             ��      g                               ,�  g   �  l�         4��                           4�          �  ��      ��                  �  �  �              �!�                    O   ����    e�          O   ����    R�          O   ����    ��          /   �  `�                                 3   ����@c    ��                              ��                          ����                                        ��              D      p�                      g                               `�      H�  ��      \c      4   ����\c                8�                      ��                    @                  8�                         X�  lc  @                     �c  @         �c          �c  @         �c              � ߱        d�  $     ��  ���                       `�  g     |�         n�      }                      D�          �  ��      ��                       ,�              ��                    O   ����    e�          O   ����    R�          O   ����    ��      ��  /    p�                                 3   �����c          ��  ��      �c      4   �����c      O    ������  d    ��                            ����                                        ��              E      ��                      g                               4�  g   %  x�         !��         0d                  l�          �  ��      ��                  %  '  (�              �                    O   ����    e�          O   ����    R�          O   ����    ��      <d  @                         � ߱            $  &  @�  ���                         ��                            ����                                        ��              F      ��                      g                               p�  /   *  `�                                 3   ����Dd        1  ��  �      `d      4   ����`d                ��                      ��                  1  >                  �ü                       1  ��                ��          ��  ��      ��                 5  <                  Tļ                       5  �      O   5    ��          O   5    ��       �  /   9  ��                                 3   ����xd        :  �  ,�      �d      4   �����d      k   ;  H�              }       n        �   H�  g   K  x�         }��                           @�          �  ��      ��                 K  W  (�              �                    O   ����    e�          O   ����    R�          O   ����    ��      ��    L  \�  l�      �d      4   �����d      O   L  ��  ��  �d     �      X�  ��                      ��        0         N  V                  \�    
  te            N  ��      $  N  ,�  ���                       �d      
                   � ߱        ��  $  N  ��  ���                       ,e      
                   � ߱            4   ����Te  �  $  P  ��  ���                       �e     
 
                   � ߱        p�  $   Q  D�  ���                       �e  @         �e              � ߱        ��  $   S  ��  ���                       �e  @         �e              � ߱            $   T  ��  ���                       f  @         f              � ߱          ��                              ��                          ����                                        ��              G       �                      g                                 X�      ��  �                      ��        0         Y  _                  ��    
  �f     L�     Y  ��      $  Y  ��  ���                       0f      
                   � ߱        �  $  Y  ��  ���                       `f      
                   � ߱            4   �����f  p�  $  Z  D�  ���                       �f     
 
    �f             � ߱        ��  $  [  ��  ���                       �f     
 
                   � ߱              ]  ��  ��      g      4   ����g      $   ]   �  ���                       Tg  @         @g              � ߱            $  a  x�  ���                       `g      
                   � ߱        adm-create-objects  ��  ��              �      H     4                          0  �!                     carga-grupos    ��  �          �-  �-  ! $ I     PG             xH          LG  [#                     carga-usuarios  $�  ��                      J      �             �	              ~#                     cargar-treeview ��  ��  �       4  h  ) ' K    �  d                      �  �#                     disable_UI  ��  X�                      L      <                              �#  
                   enable_UI   d�  ��                      M      �             �              �#  	                   excel-perfil-todos-los-usuarios ��  (�              D
    * N     �
             @          �
  $                     exitObject  H�  ��                      O      �                               -$  
                   get-usuario-opciones-menu   ��  �  �       t!  �!  , + P     T;                          P;  >$                     initializeObject    (�  ��                      Q      ,                              X$                     tvNodeAddOnExpand   ��  ��  �       �  �  . - R    �  �                      �  �%                     tvNodeCreatePopup   �  d�  �           �    / S     �                          �  ]&                     tvNodeDropEnd   x�  ��  �           �
    0 T                                �  	'                     tvnodeEvent ��  @�  �                1 U     	                          	  �(                     tvNodeSelect    L�  ��  �           ,    2 V     P                          L  �(                      �<Todos>����� �   �� ���  �       � ��    / ? O _ o  ���������������������������������������������������������������������������������������������������������������������������������������������������������                �  8   ����   0�  8   ����   @�  8   ����   P�  8   ����   `�  8   ����   p�  8   ����   ��  8   ����	   ��  8   ����	   ��  8   ����&   ��  8   ����&   ��  8   ����%   ��  8   ����%   ��  8   ����#   ��  8   ����#   h�  #  �  8   ����"   �  8   ����"   (�  8   ����   8�  8   ����   H�  8   ����   X�  8   ����   p�  8   ����   ��  8   ����             8   ����       8   ����       ��  ��      toggleData  ,INPUT plEnabled LOGICAL    ��  ��  ��      showMessageProcedure    ,INPUT pcMessage CHARACTER,OUTPUT plAnswer LOGICAL  ��  4�  @�      returnFocus ,INPUT hTarget HANDLE   $�  h�  |�      repositionObject    ,INPUT pdRow DECIMAL,INPUT pdCol DECIMAL    X�  ��  ��      removeLink  ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  �  (�      removeAllLinks  ,   �  <�  L�      modifyUserLinks ,INPUT pcMod CHARACTER,INPUT pcLinkName CHARACTER,INPUT phObject HANDLE ,�  ��  ��      modifyListProperty  ,INPUT phCaller HANDLE,INPUT pcMode CHARACTER,INPUT pcListName CHARACTER,INPUT pcListValue CHARACTER    ��  0�  <�      hideObject  ,    �  P�  h�      editInstanceProperties  ,   @�  |�  ��      displayLinks    ,   l�  ��  ��      createControls  ,   ��  ��  ��      changeCursor    ,INPUT pcCursor CHARACTER   ��   �  �      applyEntry  ,INPUT pcField CHARACTER    ��  8�  H�      adjustTabOrder  ,INPUT phObject HANDLE,INPUT phAnchor HANDLE,INPUT pcPosition CHARACTER (�  ��  ��      addMessage  ,INPUT pcText CHARACTER,INPUT pcField CHARACTER,INPUT pcTable CHARACTER ��  �  �      addLink ,INPUT phSource HANDLE,INPUT pcLink CHARACTER,INPUT phTarget HANDLE ��  `�  p�      unbindServer    ,INPUT pcMode CHARACTER P�  ��  ��      startServerObject   ,   ��  ��  ��      runServerObject ,INPUT phAppService HANDLE  ��  ��  �      restartServerObject ,   ��  $�  <�      initializeServerObject  ,   �  P�  d�      disconnectObject    ,   @�  x�  ��      destroyServerObject ,   h�  ��  ��      bindServer  ,   ��  ��  ��      processAction   ,INPUT pcAction CHARACTER   ��  ��  �      enableObject    ,   ��   �  0�      disableObject   ,   �  D�  P�      applyLayout ,   4�  d�  p�      viewPage    ,INPUT piPageNum INTEGER    T�  ��  ��      viewObject  ,   ��  ��  ��      toolbar ,INPUT pcValue CHARACTER    ��  ��  ��      selectPage  ,INPUT piPageNum INTEGER    ��  (�  <�      removePageNTarget   ,INPUT phTarget HANDLE,INPUT piPage INTEGER �  x�  ��      passThrough ,INPUT pcLinkName CHARACTER,INPUT pcArgument CHARACTER  h�  ��  ��      notifyPage  ,INPUT pcProc CHARACTER ��   �  �      initPages   ,INPUT pcPageList CHARACTER ��  8�  T�      initializeVisualContainer   ,   (�  h�  t�      hidePage    ,INPUT piPageNum INTEGER    X�  ��  ��      destroyObject   ,   ��  ��  ��      deletePage  ,INPUT piPageNum INTEGER    ��  ��  �      createObjects   ,   ��   �  0�      constructObject ,INPUT pcProcName CHARACTER,INPUT phParent HANDLE,INPUT pcPropList CHARACTER,OUTPUT phObject HANDLE �  ��  ��      confirmExit ,INPUT-OUTPUT plCancel LOGICAL  ��  ��  ��      changePage  ,   ��   �  �      assignPageProperty  ,INPUT pcProp CHARACTER,INPUT pcValue CHARACTER      � 
"     
 �%     adecomm/as-utils.w 
"   
   �    }        �
"     
   4         %              4       %              4       %              4       %              4         %              4       %              4       %              4         %              4       %              4       %                  �     }        �G� �   �G%              � �     %       	 %       	%       	 %       	%       	 %       	%               %               %               %              %              %              %               %              
�        
" 
  
 �
�    
" 
  
 �
" 
  
 _    �        P     �        \    
" 
  
   �        �         �     }        �%              
" 
  
 �
" 
  
 _    �        �     �        �    
" 
  
   �        0         �     }        �%              � 
"    
 _%              � �  �         �      T     @     $              
�    � �   _     
" 
  
 �� �   _     
�             �G                      
�            � �   �
"    
 _
�H T   %              �     }        �GG %              � 
" 
  
   P �L 
�H T   %              �     }        �GG %              
" 
  
   �        (    7%               
" 
  
 ��           \    1� �  
 �� �   _%               o%   o           � �    �
" 
  
 ��           �    1� �   �� �   _%               o%   o           � �   �
" 
  
 ��           D    1�    
 �� �   _%               o%   o           �    �
" 
  
 ��           �    1�    �� �   _%               o%   o           � %   �
" 
  
 ��           ,    1� ,   �� �   _%               o%   o           � ;   �
" 
  
 ��           �    1� R   �� ^   _%               o%   o           %               
" 
  
 _�          	    1� f   _� v     
" 
  
 ��           X	    1� }   �� �   _%               o%   o           � �  e �
" 
  
 ��           �	    1� �   �� �   _%               o%   o           �   [ �
" 
  
 ��           @
    1� a   �� ^   _%               o%   o           %               
" 
  
 ��           �
    1� q   �� ^   _%               o%   o           %               
" 
  
 ��           8    1� �   �� ^   _%               o%   o           %              
" 
  
 _�          �    1� �   _� ^     
" 
  
 ��           �    1� �  
 �� ^   _%               o%   o           %               
" 
  
 ��           l    1� �   �� �   _%               o%   o           � �    �
" 
  
 _�          �    1� �   _� v     
" 
  
 ��               1� �   �� �   _%               o%   o           � �  t �
" 
  
 _�          �    1� M  
 _� v     
" 
  
 ��           �    1� X   �� �   _%               o%   o           � i  � �
" 
  
 ��           @    1� �   �� �   _%               o%   o           � �    �
" 
  
 ��           �    1�   
 ��    _%               o%   o           %               
" 
  
 ��           0    1�    �� ^   _%               o%   o           %               
" 
  
 ��           �    1� $   �� �   _%               o%   o           � �    �
" 
  
 ��                1� 5   �� �   _%               o%   o           o%   o           
" 
  
 ��           �    1� E  
 �� �   _%               o%   o           � �    �
" 
  
 ��               1� P   �� a  	 _%               o%   o           � k  / �
" 
  
 _�          �    1� �   _� a  	   
" 
  
 ��           �    1� �   �� a  	 _o%   o           o%   o           � �    �
" 
  
 _�          4    1� �   _� a  	   
" 
  
 ��           p    1� �   �� a  	 _o%   o           o%   o           � �    �
" 
  
 _�          �    1� �   _� ^     
" 
  
 _�               1� �   _� a  	   
" 
  
 _�          \    1� �   _� a  	   
" 
  
 _�          �    1�    _� a  	   
" 
  
 ��           �    1�    �� ^   _o%   o           o%   o           %              
" 
  
 _�          P    1� &   _� a  	   
" 
  
 _�          �    1� 4  
 _� ?     
" 
  
 _�          �    1� G   _� a  	   
" 
  
 _�              1� V   _� a  	   
" 
  
 _�          @    1� i   _� a  	   
" 
  
 _�          |    1� ~   _� a  	   
" 
  
 _�          �    1� �  	 _� a  	   
" 
  
 _�          �    1� �   _� a  	   
" 
  
 _�          0    1� �   _� a  	   
" 
  
 ��           l    1� �   �� �   _%               o%   o           o%   o           
�H T   %              �     }        �GG %              
" 
  
   
" 
  
 �
" 
  
   
" 
  
 _(�  L ( l       �        4    �� �   � P   �        @    �@    
� @  , 
�       L    �� �     p�               �L
�    %              � 8      X    � $         � �          
�    � �     
" 
  
 �� @  , 
�       h    ��    
 �p�               �L"      P �L 
�H T   %              �     }        �GG %              
" 
  
 ��               1� �  
 �� �   _%               o%   o           � �    �
" 
  
 ��           �    1�   
 �� �   _%               o%   o           o%   o           
" 
  
 ��               1�    �� v   _%               o%   o           o%   o           
" 
  
 ��           �    1�    �� ^   _%               o%   o           %               
" 
  
 ��           �    1� (   �� ^   _%               o%   o           %               
" 
  
 ��           x    1� 5   �� �   _%               o%   o           � �    �
" 
  
 ��           �    1� <   �� ^   _%               o%   o           %              
" 
  
 ��           h    1� N   �� ^   _%               o%   o           o%   o           
" 
  
 ��           �    1� Z   �� �   _%               o%   o           o%   o           
" 
  
 ��           `    1� h  	 �� �   _%               o%   o           � �    �
" 
  
 ��           �    1� r   �� �   _%               o%   o           o%   o           
" 
  
 ��           P    1� �   �� �   _%               o%   o           o%   o           
" 
  
 ��           �    1� �   �� ^   _%               o%   o           %               
" 
  
 ��           H    1� �   �� ^   _%               o%   o           o%   o           P �L 
�H T   %              �     }        �GG %              
" 
  
 ��                1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��           �     1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��            !    1� �   �� ^   _%               o%   o           %               
" 
  
 ��           |!    1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��           �!    1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��           d"    1� �   �� ^   _%               o%   o           %               
" 
  
 ��           �"    1�    �� a  	 _%               o%   o           � �    �
" 
  
 ��           T#    1�    �� a  	 _%               o%   o           � �    �
" 
  
 ��           �#    1� #   �� a  	 _%               o%   o           � �    �
" 
  
 ��           <$    1� 1   �� a  	 _%               o%   o           o%   o           
" 
  
 ��           �$    1� ?   �� a  	 _%               o%   o           � �    �
" 
  
 ��           ,%    1� O   �� a  	 _%               o%   o           � �    �
" 
  
 ��           �%    1� ]  	 �� ?   _%               o%   o           %               
" 
  
 ��           &    1� g   �� ?   _%               o%   o           %               
" 
  
 ��           �&    1� p   �� ^   _%               o%   o           o%   o           
" 
  
 ��           '    1� �   �� ^   _%               o%   o           o%   o           
" 
  
 ��           �'    1� �   �� ^   _%               o%   o           %               
" 
  
 ��           (    1� �   �� ^   _%               o%   o           %               
" 
  
 ��           �(    1� �   �� ^   _%               o%   o           %               
" 
  
 ��           )    1� �   �� �   _%               o%   o           %       
       
" 
  
 ��           �)    1� �   �� �   _%               o%   o           o%   o           
" 
  
 ��           �)    1� �   �� �   _%               o%   o           %              
" 
  
 ��           x*    1� �   �� �   _%               o%   o           o%   o           
" 
  
 ��           �*    1� �   �� �   _%               o%   o           %              
" 
  
 ��           p+    1� 	   �� �   _%               o%   o           o%   o           
" 
  
 ��           �+    1�    �� �   _%               o%   o           %              
" 
  
 ��           h,    1�    �� �   _%               o%   o           o%   o           
" 
  
 ��           �,    1� &   �� a  	 _%               o%   o           � �    �P �L 
�H T   %              �     }        �GG %              
" 
  
 ��           �-    1� 8   ��    _%               o%   o           %               
" 
  
 ��           (.    1� D   ��    _%               o%   o           o%   o           
" 
  
 ��           �.    1� P   �� �   _%               o%   o           � �    �
" 
  
 ��           /    1� `   �� �   _%               o%   o           � v  - �
" 
  
 ��           �/    1� �   �� �   _%               o%   o           � �    �
" 
  
 ��            0    1� �   �� �   _%               o%   o           � �   �
" 
  
 _�          t0    1� �   _� v     
" 
  
 ��           �0    1�    �� �   _%               o%   o           � �    �
" 
  
 _�          $1    1�   
 _� v     
" 
  
 _�          `1    1�    _� v     
" 
  
 ��           �1    1� +   �� a  	 _%               o%   o           � �    �
" 
  
 ��           2    1� 8   �� �   _%               o%   o           � �    �
" 
  
 ��           �2    1� E   �� v   _%               o%   o           o%   o           
" 
  
 ��            3    1� R   �� �   _%               o%   o           � e  ! �
" 
  
 ��           t3    1� �   �� �   _%               o%   o           � �    �
" 
  
 ��           �3    1� �   �� �   _%               o%   o           � �   �
" 
  
 ��           \4    1� �  	 ��    _%               o%   o           o%   o           
" 
  
 ��           �4    1� �   �� ^   _%               o%   o           %               
" 
  
 _�          T5    1� �   _� v     
" 
  
 ��           �5    1� �   �� �   _%               o%   o           � �   �
" 
  
 ��           6    1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��           x6    1� 
   �� a  	 _%               o%   o           � �    �
" 
  
 _�          �6    1�    _� v     
" 
  
 _�          (7    1� ,   _� a  	   
" 
  
 ��           d7    1� ?   �� ^   _o%   o           o%   o           %               
" 
  
 _�          �7    1� V   _� ^     
" 
  
 _�          8    1� m   _� a  	   
" 
  
 _�          X8    1� {   _� a  	   
" 
  
 _�          �8    1� �   _� a  	   
" 
  
 _�          �8    1� �   _� a  	   
" 
  
 _�          9    1� �   _� a  	   
" 
  
 _�          H9    1� �   _� v     
" 
  
 ��           �9    1� �   �� �   _%               o%   o           � �  4 �
" 
  
 _�          �9    1�    _� v     
" 
  
 _�          4:    1� +   _� v     
" 
  
 _�          p:    1� ;   _� v     
" 
  
 _�          �:    1� H   _� a  	   
" 
  
 _�          �:    1� \   _� a  	   
" 
  
 _�          $;    1� n   _� a  	   
" 
  
 _�          `;    1� �   _� ^     
" 
  
 ��           �;    1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��           <    1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��           �<    1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��           �<    1� �   �� a  	 _%               o%   o           � �    �
" 
  
 ��           l=    1� �   �� ^   _%               o%   o           %               
" 
  
 ��           �=    1� �   �� ^   _%               o%   o           o%   o           
" 
  
 ��           d>    1� �   �� ^   _%               o%   o           %               
" 
  
 ��           �>    1�    �� ^   _%               o%   o           %               
" 
  
 ��           \?    1�    �� ^   _%               o%   o           o%   o           
" 
  
 ��           �?    1� (   �� ^   _%               o%   o           %               
" 
  
 _�          T@    1� 6   _� a  	   
" 
  
 ��           �@    1� D   �� ^   _%               o%   o           %              
" 
  
 _�          A    1� U   _� a  	   
" 
  
 _�          HA    1� a   _� a  	   
" 
  
 _�          �A    1� p  
 _� a  	   
" 
  
 ��           �A    1� {   �� a  	 _%               o%   o           � �   �
" 
  
 ��           4B    1� �   �� a  	 _%               o%   o           � �    �
" 
  
    " 
 	  _%     start-super-proc w_%     adm2/smart.p _P �L 
�H T   %              �     }        �GG %              
" 
  
   �       TC    6� �     
" 
  
   
�        �C    8
" 
  
   �        �C    ��     }        �G 4              
" 
  
 ߱G %              G %              %p e `   LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout _
�H T   %              �     }        �GG %              
" 
  
 _
" 
  
 _
" 
  
 _
" 
  
   (�  L ( l       �        �D    �� �   � P   �        �D    �@    
� @  , 
�        E    �� �   _p�               �L
�    %              � 8      E    � $         � �          
�    � �   _
" 
  
 �p� @  , 
�       F    �� }   �p�               �L" 
   , �   � �   �� �   _�     }        �A      |    " 
     � �   �%              (<   \ (    |    �     }        �A� �   �A" 
   �    " 
   _" 
   �  < " 
   _" 
   �(    |    �     }        �A� �   �A" 
   �
�H T   %              �     }        �GG %              
" 
  
 _
" 
  
 _
" 
  
 _
" 
  
   (�  L ( l       �        �G    �� �   � P   �        �G    �@    
� @  , 
�       H    �� �   _p�               �L
�    %              � 8      H    � $         � �          
�    � �   _
" 
  
 �p� @  , 
�       $I    �� �  
 �p�               �L" 
   , 
�H T   %              �     }        �GG %              
" 
  
 _
" 
  
 _
" 
  
 _
" 
  
   (�  L ( l       �        �I    �� �   � P   �        �I    �@    
� @  , 
�       �I    �� �   _p�               �L
�    %              � 8      �I    � $         � �          
�    � �   _
" 
  
 �p� @  , 
�       �J    �� f   �p�               �L
" 
  
 , 
�H T   %              �     }        �GG %              
" 
  
   
" 
  
 �
" 
  
   
" 
  
   (�  L ( l       �        �K    �� �   � P   �        �K    �@    
� @  , 
�       �K    �� �     p�               �L
�    %              � 8      �K    � $         � �          
�    � �     
" 
  
 �p� @  , 
�       �L    ��    
 �p�               �L%     SmartWindow 
" 
  
   p� @  , 
�       8M    ��      p�               �L%      WINDOW  
" 
  
  p� @  , 
�       �M    �� �    p�               �L%               
" 
  
  p� @  , 
�       �M    �� �    p�               �L(        � �      � �      � �      �     }        �A
�H T   %              �     }        �GG %              
"   
 � (   � 
"   
 _    �        �N    �� �   �
"   
   � 8      $O    � $         � �          
�    � �   _
"   
   �        |O    �
"   
   �       �O    /
"   
   
"   
   �       �O    6� �     
"   
   
�        �O    8
"   
   �        P    �
"   
   �       4P    �
"   
   p�    � �   �
�    �     }        �G 4              
"   
 ߱G %              G %              
�     }        �
"   
    (   � 
"   
 _    �        �P    �A"    �A
"   
   
�        DQ    �@ � 
"   
 �"      �       }        �
"   
 _%              %                " 
 	  _%     start-super-proc v_%     adm2/appserver.p I��    � x     
�    �     }        �%               %      Server  - �     }        �    " 
   �� �    _%                   " 
   �� �    _%      NONE    p�,  8         $     " 
   �        � �   _
�    
�H T   %              �     }        �GG %              
" 
  
 _
" 
  
 _
" 
  
 _
" 
  
   (�  L ( l       �        �S    �� �   � P   �        �S    �@    
� @  , 
�       �S    �� �   _p�               �L
�    %              � 8      �S    � $         � �          
�    � �   _
" 
  
 �p� @  , 
�       �T    �� r   �p�               �L" 
   , p�,  8         $     " 
   �        � �   _
�     " 
 	  _%     start-super-proc u_%     adm2/visual.p _�   � �     � �     � �  U   
�H T   %              �     }        �GG %              
" 
  
 _
" 
  
 _
" 
  
 _
" 
  
   (�  L ( l       �        V    �� �   � P   �         V    �@    
� @  , 
�       ,V    �� �   _p�               �L
�    %              � 8      8V    � $         � �          
�    � �   _
" 
  
 �p� @  , 
�       HW    ��    �p�               �L" 
   , � 
"    
 _%     contextHelp 
"    
   
�    
�    %     processAction   
�    %     CTRL-PAGE-UP _%     processAction   
�    %     CTRL-PAGE-DOWN  " 
 	  _%     start-super-proc t_%     adm2/containr.p %     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%      initializeDataObjects �0 0   A    �    � b   �
�    � t   _A    �    � b     
�    � �   _%     modifyListProperty 
�    
�    %      Add     %      ContainerSourceEvents �%     buildDataRequest ent0 A    �    � b   _
�    � �   �%     modifyListProperty 
�    
�    %      Add     %     SupportedLinks %      ContainerToolbar-Target � 
"    
 _
" 
  
 _%     contextHelp 
"    
   
�    
�    %               
�H T   %              �     }        �GG %              
" 
  
 _
" 
  
 _
" 
  
 _
" 
  
 �(�  L ( l       �        �[    �� �   � P   �        �[    �@    
� @  , 
�       �[    �� �   _p�               �L
�    %              � 8      �[    � $         � �   _     
�    � �   _
" 
  
 �p� @  , 
�       �\    ��    �p�               �L
�             �G
�H T   %              �     }        �GG %              
" 
  
 _
" 
  
 _
" 
  
 _
" 
  
 _(�  L ( l       �        t]    �� �   � P   �        �]    �@    
� @  , 
�       �]    �� �   _p�               �L
�    %              � 8      �]    � $         � �   _     
�    � �   _
" 
  
 �p� @  , 
�       �^    �� �   �p�               �L%              (        �     }        �G� �   �G� 
" 
  
 _
" 
  
   �        H_    �%              
" 
  
 _
" 
  
 _�     }        �%               
" 
  
 _%      CLOSE   %               �    }        �� �      �    }        �� �     %     carga-grupos    �    }        �� �      4         %              4         %              4         %              4         %              %     lib\Tools-to-excel  8      $   � �     4  ߱     %              � 
      %     pi-crea-archivo-csv 
"   
   
�     
        �G"      "      %     pi-crea-archivo-xls 
"   
   
�     
        �G"      "      
"   
   %(     excel-perfil-todos-los-usuarios " 
     � &         "      &    "      4   &    $    4          %              &    4       %              4       %              4       %              %     carga-usuarios  � 
" 
  
 _
" 
  
 �
" 
  
 _�        xc    %%              
�     }        �
" 
  
   %     destroyObject       �     }        �    �  �    	   %               
" 
  
 _
�    %     createObjects    �     }        �%     initializeObject t_ �     }        �$    4         %              � e      %                   %              %                   " 
     %                  " 
     " 
     " 
    
4    
  
   " 
     
" 
  
   �        �e    `" 
     
" 
  
   �        �e    `%              
" 
  
   �        �e    
`%                  %              %                   " 
     %                  " 
     �            �'�            �'
�           >" 
     " 
     
4    
  
   " 
         " 
   �%               
" 
  
   �        4g    �g" 
     �            �'�     "       %               %     constructObject %     pure4gltv.w 
�             �G%`TP  wineModeAutomaticwindowsSkinRoyalepicCacheCoef1labCacheCoef1tvIterationHeight17TreeStyle3FocSelNodeBgColor1UnfSelNodeBgColor8tvnodeDefaultFont1FocSelNodeFgColor15UnfSelNodeFgColor0resizeVerticalyesresizeHorizontalyesDragSourcenoneautoSortnoMSkeyScrollForcePaintyesHideOnInitnoDisableOnInitnoObjectLayout N  
" 
  
   %     repositionObject t_
" 
  
   %         %          %     resizeObject    
" 
  
   %        %           %      addLink 
" 
  
   %     tvNodeEvent 
�    %     adjustTabOrder  
" 
  
   
�            �G%      AFTER   % 	    emptyTree _
" 
  
   �            NA� �      �            NA� �      *    4       %                  " "    &    z     " "         %              %                   " !     %                   " !     �    " !     � /"     �    " !     � /"    T   " !     " !     � /"     %              " "     %              " "   j&    &    * #   " #     %              D  (   %                  �            F� �    F�           �            F�              � &   _� &     �            B� &     � &     4       %                  " "    &    " "   j&    &    � F"     * #   " #     �              " !     " "      (   %                  �            B� �    B�            B" !     4       %                  " "    &    z     " "     " "     4         %                  %              %                   " !     %                  " !     %       ,      %       ,      � �      " !         %              %                   " !     %                  " !     %              %              � �      " !     � �      " !     � �"   �%              � �    _%              %              " "   j&    &    " !   j&    &     " %     (        " $     %              %                   " $     %              � �      � �      � �      t 8   P      ( " %   ߱%                  " $     %              4    !     " $         4    !     " $     %                  " %     � �          %              %                   " !     %                  " !     �     " !     �     " !     A     " %     T    " !     " !     %                   " !   �� �"   _%               " ! 	  _� �"     " %   j� #     " %     " %     � e      " %     " %     " %     � #         " $   �%              " %     U    � #     %       x       � #  	       " $   �%              " %     � #     � 3#         " $   �%              " %     U    � A#     %       x           " !   �� �"   _" !     %              � �      %              " ! 
    %              " !     %              " %     %                   " !     " ! 
    %       
       * #   " #     %              4    $     " $          " $     %              " %          " $     %                   " $     %              �            NA d     8 $    $    4  NA     %              � C#     4         %              � E#     �            NA x     L $    8      $   � G#     4         %              � C#     4         %              � E#     %     cargar-treeview � �      "      "      � &         "      &    "      4   &    $    4          %              &    4       %              4       %              4       %              ( X       " 
     %               ( (       " &     %                   " &     %              " &     %              " &     %              � n#     %                  " &     %              � u#     %              %              &    4       %              %     carga-grupos        " '   �� �    _4  (     %              (P   $ $   4       %              4    (     %              %               %                  " )    %              %      addNode 
" 
  
   4       %              � �     4         %              � �#    � �#    p�    � �#  	 �
" 
  
 _(        �     }        �G� �   �G� 
" 
  
 _
" 
  
   �     }        �
�    
" 
  
 _" 
   �" 
   _
" 
  
 �� &         "      &    "      4   &    $    4          %              &    4       %              4       %              4       %              %              &    4       %              "      "      
" 
  
   �    }        �� �     *    %$     get-usuario-opciones-menu _4       %              4         %              4         %              4         %              4         %              4         %              4         %              4         %              %     lib\Tools-to-excel � �#  %   %     pi-crea-archivo-csv 
" *  
   
�     
   	     �G" *     " *     %     pi-crea-archivo-xls 
" *  
   
�     
   	     �G" *     " *     
" *  
   �    }        �� �      %              &    4       %              %      CLOSE   %               " +   j    " "     &    z     " "         %              %                   " ,     %                   " ,     �    " ,     � /"     �    " ,     � /"    T   " ,     " ,     � /"     %              " "     %              " "   j&    &    * #   " #     %              " +   j    " "     &    z     " "     " "     " +         %              %                   " ,     %                  " ,     %       ,      %       ,      � �      " ,         %              %                   " ,     %                  " ,     %              %              � �      " ,     � �      " ,     � �"   �%              � �    _%              %              " "   j&    &    " ,   j&    &     " %     (        " +     %              %                   " +     %              � �      � �      � �      t 8   P      ( " %   ߱%                  " +     %              4    ,     " +         4    ,     " +     %                  " %     � �          %              %                   " ,     %                  " ,     �     " ,     �     " ,     A     " %     T    " ,     " ,     %                   " ,   �� �"   _%               " , 	  _� �"     " %   j� #     " %     " %     � e      " %     " %     " %     � #         " +   �%              " %     U    � #     %       x       � #  	       " +   �%              " %     � #     � 3#         " +   �%              " %     U    � A#     %       x           " ,   �� �"   _" ,     %              � �      %              " , 
    %              " ,     %              " %     %              * #   " #     %              4    +     " +          " +     %              " %          " +     %                   " +     %              %      SUPER   %     carga-usuarios  " -     8$    4         %       
       &    4       %       
       (    $ 4         %       
        " -     , (         " -     %              $ 4         %       
       � �$     � �      � �      $    4         %              � e      � �      $    4         %              � e      � �$     $    4         %              � e      4         %              $    4         %              � #     � �#     $    4         %              � #     � �$     $    4         %              � #     4         %              %      addNode 
" 
  
   4       %       
       " -    " .    " .    " .        " -   �� %   _%      addNode 
" 
  
   � %     � %     � %  	   � %     � .%     %      addNode 
" 
  
   � 7%     � %     � =%  	   � %     � �#         " -   �� 7%   _%      addNode 
" 
  
   � G%     � 7%     � N%  
   � Y%     � �      8    " -   �� i%   _8    " -   �� m%   _8    " -   �� s%   _8    " -   �� z%  	 _T   %              " -     � �%   �%     loadDirectory   " -     " -       <   8    " /     � �%   _     �    " /     � �%   _%              � �%  B   8    " /   �� i%   _� �%  3   8    " /   �� m%   _� &&     8    " /   �� s%   _� =&     ( T    %              " 0     ( T    %              " 0     8    " 0   �� �&   _�P  \         $     " 0   ߱                $     " 0   _        � �&   �
" 
  
 _    %              %                   " 0     %                  " 0     �     " 0     �     " 0     
�  T    " 0     " 0   ߱
" 0  
   
" 0  
 _
" 0  
 _   �        " 0     � �&     (0 \      �        ؒ    �%              (  (  �    
" 0  
 �� �&   _     � �&   ��        �    B� �&   B�        �    �
%   
               %              %                   " 0     %                  " 0     �     " 0     �     " 0     
�  T    " 0     " 0   ߱  �    
" 0  
 _� �&   �
" 0  
 _ �        ��     %     getNodeDetails  
" 
  
   " 0     
" 0 
 
    � 
" 0 
 
 �
" 0  
 _�        4�     � �&     
" 0  
   �        `�    
" 0  
 �     �        ��    %              
" 0  
   
" 0 
 
 _� T      ��     @    � @  , 
�       ȕ    �� �&   _p�               �L� �&   �L� �&     
" 0  
   �        H�    B
" 0  
 �
" 0 
 
     @   �        h�    B� @  , 
�       t�    �� �&     p�               �L
" 0  
   �        ܖ    B
" 0 
 
 _� @  , 
�       ��    �� �&   _p�               �L� �&     
" 0  
   
" 0 
 
   " 1     � �#     %     tvNodeaddOnExpand _" 1     � '     %     tvNodeSelect    " 1     � '  
   %     tvNodeCreatePopup _" 1      �     }        ��  � )'  4   �  � ^'     � o'  	   � y'     � �'     � �'     � �'  O   � �'     � (     %      
            � (  '   " 1   _� ;(  	   8    " 1   �� �&   _� E(         " 1   �� T(   _� W(  
       " 1   �� b(   _� r(     
" 1 	 
   � 
" 1 	 
 _     
" 1 	 
 �     
�             �G8    " 1   �� �(   _%     tvNodeDropEnd   " 1     " 1     8    " 2   �� �(  	 _8    " 2   �� �(  
 _                �           �   l       ��                 �  �  �               ̞�                    O   ����    e�          O   ����    R�          O   ����    ��        $  �  �   ���                       �N     
                    � ߱              �  (  �      �N      4   �����N                �                      ��                  �  �                  ؖ�                       �  8  �  �  �  0O            �  �  `      �O      4   �����O                p                      ��                  �  �                  d��                       �  �  �  o   �      ,                                 �  �   �  �O      �  �   �  �O      $  $  �  �  ���                        P     
                    � ߱        8  �   �   P      L  �   �  @P      `  �   �  `P          $   �  �  ���                       �P  @         |P              � ߱                     T          ,  @   T �            
             
             
             
                 $   4   D          $   4   D   ����        ��                            ����                                            �           �   l       ��                   P  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��                            �          �  $  !    ���                       �P     
                    � ߱                  �  �                      ��                   "  $                  ���                     "  4      4   ����Q      $  #  �  ���                       PQ     
                    � ߱        �    %  4  D      dQ      4   ����dQ      /  &  p                               3   ����xQ  �  �   A  �Q          O   N  ��  ��  �Q                               , �                          
                               �      ��                            ����                                            �           �   l       ��                 j  �  �               �$�                    O   ����    e�          O   ����    R�          O   ����    ��      tg                          � ߱          $  r  �   ���                           p   t  |g  (      �      �     �g                �                      ��                  v  �                  ��                       v  8    /   w  �     �                          3   �����g                                 3   �����g  P     
   @                      3   �����g  �        p                      3   �����g         
   �  �                  3   ����Hi      $   w  �  ���                               
 
                   � ߱        �  /	  |  4     D  ti                      3   ����Ti  t        d                      3   �����i            �                      3   �����i  @  /	  }  �     �  �i                      3   �����i                                 3   �����i            0                      3   �����i    /   �  l     |                          3   �����i  �     
   �                      3   ����j  �        �                      3   ����j         
   �                      3   ����0j      /   �  8     H                          3   ����8j  x     
   h                      3   ����Tj  �     
   �                      3   ����`j            �                      3   ����tj                ,            $                                                  ��                              ��                          ����                                            �           �   l       ��PH               �  c  �               �	�                    O   ����    e�          O   ����    R�          O   ����    ��      �   �   �     �   �   �       /  �            �j                      3   �����j  h  $   �  <  ���                       �j  @         �j              � ߱        �  $   �  �  ���                       �j  @         �j              � ߱        t)    �  �  X      �j      4   �����j                �                      ��                  �  \                  lƼ                       �  �        �      �          T  <      ��                  �  �  l              ʼ                	     �  h        `       ��                            7   ����    "     ����               k    �            �                  6   �       " �  ����         �  k    �            �                                                        �j                 (                                     @            �           O   ����  e�          O   ����  R�          O   ����  ��      H  $  �  �  ���                       4k      !                   � ߱          X      �  `  hH      0        ��       0         �  �  H              ��    !  �k            �  �      $  �  �  ���                       Hk      !                   � ߱          $  �  �  ���                       xk      !                   � ߱            4   �����k      O   ����  e�          O   ����  R�          O   ����  ��      �  9   �     �k           l         4l          @l             � ߱        �  $  �  p  ���                       �  A  �       # @   ��         4                                             Tl                 �  |           `l           hl         �            \   l          �  �  4      pl      4   ����pl                p                      ��                  �  �                  ��                       �  �  xl          �l             � ߱            $  �  D  ���                                     	                      ��                  �  �                  ��                L     �  �  X	    �  4	  D	      �l      4   �����l      �  �  �l      �	  �   �  m      Lm  @         8m              � ߱        �	  $   �  l	  ���                       �
  $  �  �	  ���                       Xm                         � ߱              �
      8            �      ��                  �  �                 D�                       �  
      �
         ��                            7   ����    "     ����               �m    �            d                  6   �       " �  ����         �  �m    �            d                                                        dm                 �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  A  �       # �   ��         �                                             �m                 �  �           �m           �m         �            �   �    H  $  �    ���                       �m      !                   � ߱        �    �  d  t      �m      4   �����m      $  �  �  ���                       �m      !                   � ߱        �  �   �  �m            �  �  x      n      4   ����n                �                      ��                  �  �                  @�                       �    hn  @         Tn              � ߱            $   �  �  ���                             \      �          �  �      ��                  �  W  �              ��                �(     �  �      �  �       ��                            7   ����    "     ����               �n    �            (                  6   �       " X  ����         L  �n    �            (                                                        tn                 �  �                                   @            t   �        O   ����  e�          O   ����  R�          O   ����  ��      T  $  �  (  ���                       �n      !                   � ߱        �  $  �  �  ���                       �n      !                   � ߱        p  $  �  �  ���                       �n      !                   � ߱        	  �      �  @                      ��        0    	     �  �                  ��    !  xo          �        $  �  �  ���                       �n      !                   � ߱        0  $  �    ���                       (o      !                   � ߱            4   ����Po      $  �  l  ���                       �o      $    �o             � ߱        
        l  �                      ��        0    
     �  �                  䳼    !  $p     �     �  �      $  �  @  ���                       �o      !                   � ߱        �  $  �  �  ���                       �o      !                   � ߱            4   �����o  ,  $  �     ���                       8p      !    Dp             � ߱            $  �  X  ���                       Pp      !    \p             � ߱        hp      !    tp         �p      !    �p         �p      $                   � ߱          $  �  �  ���                       8  A  �       # p   ��         d                                             �p                 �  �           �p           �p         �            �   �          H      �  XH      �  �      ��                  �  U  �              ���                       �  �      t  �       ��                            7   ����    %      ��                     �                              6   �       % D   ��         8        �                                                                    �p                 �  �           �p           �p                      `   p        O   ����  e�          O   ����  R�          O   ����  ��      @  $  �    ���                       �p      $                   � ߱        �  $  �  l  ���                       q      $                   � ߱        �  $  �  �  ���                       Hq      $                   � ߱        H  $  �    ���                       pq      !                   � ߱        �  $  �  t  ���                       |q      ! 
       
           � ߱        �  $  �  �  ���                       �q      !                   � ߱        P  $  �  $  ���                       �q      ! 	       	           � ߱        �    �  l  �      Hr      4   ����Hr                d                      ��                  �                    Լ                       �  |    t      �  4                      ��        0         �  �                  tԼ    !  �r     x     �  �      $  �  �  ���                       hr      !                   � ߱        $  $  �  �  ���                       �r      !                   � ߱            4   �����r        �  P  `      �r      4   �����r      O   �  �� ��          $     �  ���                       0s      ! 	       	           � ߱        T      �  �      Ds      4   ����Ds      $    (  ���                       ds      ! 	       	           � ߱        �      p  �      xs      4   ����xs      O     �� ��      �  $    �  ���                       �s      !                   � ߱        &  p   	  �s        D  H   �     �s                �                      ��                  
                    Dռ                       
    �  $    �  ���                       �s      ! 
       
           � ߱            $       ���                       �s      !                   � ߱        �!  �      �s                �                       ��                    %                  ,޼                         X   ,!  $     !  ���                       �s      ! 
       
           � ߱        �!  $    X!  ���                       �s      !                   � ߱            $    �!  ���                       �s      !                   � ߱        D#  �!     �s        (  "  �"       t      4   ���� t                �"                      ��                  (  +                  �޼                       (  "  �"  $  )  �"  ���                       (t      ! 
       
           � ߱            $  *  #  ���                       4t      !                   � ߱        �$  T#     \t        1  p#  �#      ht      4   ����ht                �#                      ��                  1  4                  D߼                       1  �#  T$  $  2  ($  ���                       �t      ! 
       
           � ߱            $  3  �$  ���                       �t      !                   � ߱            �$     �t        :  �$  T%      �t      4   �����t                d%                      ��                  :  =                  �߼                       :  �$  �%  $  ;  �%  ���                       �t      ! 
       
           � ߱            $  <  �%  ���                       �t      !                   � ߱        �(    F  0&  �&      u      4   ����u                �&                      ��                  F  P                  t�                       F  @&  �'  9   G     0u          <u         Pu          \u         pu          |u         �u          �u         �u          �u         �u          �u             � ߱        �'  $  H  �&  ���                             O  �'  �'      v      4   ����v      $  O  (  ���                       v          v             � ߱        ,v      !    Hv         pv      !    |v         �v      $                   � ߱            $  R  4(  ���                       )  $   Y  �(  ���                       �v  @         �v              � ߱            $   Z  H)  ���                       lw  @         Xw              � ߱        �)  /   ^  �)     �)                          3   �����w            �)                      3   ����x  t+  s   `  *                 p+              8*  �*       ��                            7   ����           ��                     �            �*                  6   `         �*   ��                    �            �*                                                                D+  8+                                   @            +   (+           x  ,x          X+      s   a  �+      p-                      �+  ,  �,                               7   ����          ����                lx   �            l,                  6   a         �,  ����               lx   �            l,                                                                �,  �,                                   @            �,   �,        J   a        ��T-    ��                                                          �x  �x  �x                      <-                 8x   Dx   `x              !  �-                                             $ ]�E          A  |C  pp.             �     ��     �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ,0  @  P  `  Xhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������		(	8	H	X	h	x	�	�	�	�	�	�	�	�	
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
�(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx�������� �T� $   / ? O _ o  � � � � � � � �                                                                                                                                                                                                                                                                                                            �      ! $   ��                              ��                           ��                             ��                             ��                             ��                             ��                            ����                                #      =   U         =   �     ^        2                 S�    �       2                 S�                    �           �   l       ���	               i  �  �               T�                    O   ����    e�          O   ����    R�          O   ����    ��      0  �   o                   l                      ��                  q  s                  ���                     q  �          
                   � ߱            $   r  @   �                                   �  t	      x  `      ��                  v  �  �              d��                �     v  �      @  �       ��                            7   ����    &      ��          	           �            �                  6   v       &    ��         	           �            �                                                                L  @                                   @                0        O   ����  e�          O   ����  R�          O   ����  ��            x  �  @      y      4   ����y                P                      ��                  x  �                  ��                       x  �  �  9   {     �y          �y         �y          �y         �y          �y             � ߱        �  $  |  `  ���                                 �      z      4   ����z                �                      ��                    �                  ���                           ,z          8z             � ߱            $  �  �  ���                       �  A  �        @   ��         
                                                        �  |                                   @            \   l    �  s   �  �       �                      �  D                                 7   ����           ��                `z   �            �                  6   �         �   ��               `z   �            �                                                                   �                                   @            �   �        J   �        ��t    ��                                                          hz                      d                 Lz  �  u   �                     /   �  �                                 3   �����z    ��                              ��                           ��                            ����                                =   �           9        2                 S�                    �           �   l       ��                 �  �  �               X��                    O   ����    e�          O   ����    R�          O   ����    ��      �#   '                   �               �  �   t      �z      4   �����z                �                      ��                  �  �                  ���                       �                 L              T  ��           �z  �  �  4              4��                       �  �      ,  |  �        L                      7   ����         (                      �            �                  6   �       �   (                     �            �                                                                8  ,                                   @                        (     8   �  (         �  4  �         �                                                                                                                                                                                (                                                                                       J   �          �    ��                                                           �z                      �              O   ����  e�          O   ����  R�          O   ����  ��            �  h  �      l{      4   ����l{                �                      ��                  �  �                  ��                       �  x      /  �        0  �{                      3   �����{  `        P                      3   �����{  �        �                      3   �����{  �        �                      3   �����{  �        �                      3   ����|                                  3   ����|      �   �   |                  )  �                                             '  �          �  �    �           ��                               ' )   ��                             ��                            ����                                            �           �   l       ��                  �  �  �               T�                    O   ����    e�          O   ����    R�          O   ����    ��           �  �   �       @|      4   ����@|      n   �     �          �|        �    ,      �|      4   �����|      �   �  �|    ��                            ����                                            �           �   l       ��                  �  �  �               0e�                    O   ����    e�          O   ����    R�          O   ����    ��      �|  �          �|  �              � ߱        �  Z   �  �    �        �|                  �               �              �              �              �              �              �              � ߱        �  h   �     �        �|              �  s   �  �       �                        d  4                               7   ����          ����                }   �            �                  6   �         �  ����               }   �            �                                                                                                      @            �           J   �        ���    ��                                                          P}  t}  �}                      �                 �|   �|    }  �  s   �  �      �                      (  x  H                               7   ����           ��                �}   �            �                  6   �         �   ��               �}   �            �                                                                4  (                                   @                       J   �        ���    ��                                                          �}                      �                 �}  d  s   �  �                `              (  x       ��                            7   ����           ��                     �            �                  6   �         �   ��                    �            �                                                                4  (                                   @                         �}  ~          H      
   �  �� �             ~    ��                              ��                          ����                            �        2                 S�    9       2                 S�    ^       2                 S�                    �           �   l       ��                 �     �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �   �   �   ~      �   �   �  	   `  u   �                           p  �                      ��                   �                    �|�                (     �  �       4   ����@~  X  /   �  �     �                          3   ����H~            �                      3   ����p~        h      �  0      �  �      ��                  �     �              �}�                     �  �      �  �       ��                            7   ����          ��                     �            4                  6   �        X   ��                    �            4                                                                �  �                                   @            t   �        O   ����  e�          O   ����  R�          O   ����  ��      �  9   �  	   �~      	               �~      	               �~      	                      	               $      	               H      	               l      	                   � ߱            $  �    ���                           u                    D  u                    �  /  
  p         *                      3   �����  �  $    �  ���                       �      *                   � ߱        �  /           �                      3   �����  D     
   4                      3   �����  t        d                      3   �����            �  �                  3   �����      $     �  ���                                *                   � ߱           /    (     8  4�                      3   �����  h     
   X                      3   ����@�  �        �                      3   ����T�            �  �                  3   ����`�      $     �  ���                                *                   � ߱        0  �     l�  D  �     x�          s     p       8
                      �  �  �	                               7   ����           ��                ��   �            <	                  6            `	   ��               ��   �            <	                                                                �	  �	                                   @            |	   �	        J           ��
    ��                                                          ��                      
                 ��             *  �
          �
  �
   @ x
            
                                                 0              0   �  *     ��                             ��                            ����                                =      	   9        2                 S�                    �           �   l       ��                  &  0  �               L��                    O   ����    e�          O   ����    R�          O   ����    ��      �     -  ؀  }          O   .  ��  ��  �    ��                            ����                                            �           �   l       ��4<               6  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      8$   +                   �          �   �   ?     h  �   @           x                �  �      ��                  E  R                 TL�                �     E  �       �  �       ��                            7   ����    "     ����               �    �            D                  6   E       " t  ����         h  �    �            D                                                         �                 �  �                                   @            �   �        O   ����  e�          O   ����  R�          O   ����  ��      �  $  F  D  ���                       (�      ,                   � ߱          �      D  �  <<      �  �      ��       0         G  P  �              pM�    ,  ȁ            G  p      $  G    ���                       <�      ,                   � ߱        �  $  G  p  ���                       l�      ,                   � ߱            4   ������      O   ����  e�          O   ����  R�          O   ����  ��      L  9   H     �          �         (�          4�             � ߱        x  $  I    ���                       0  A  L       # �   ��         �                                             H�                              T�           \�         �            �              M  L  �      d�      4   ����d�                                      ��                  M  O                  ���                       M  \  l�          x�             � ߱            $  N  �  ���                                                             ��                  T  i                  L��                     T  0        (      �	          �	  �	      ��                  }  �  �	              ���                       }  �      T  �       ��                            7   ����    "     ����               ��    �            �                  6   }       " $	  ����         	  ��    �            �                                                        ��                 l	  `	                                   @            @	   P	        O   ����  e�          O   ����  R�          O   ����  ��       
  $  ~  �	  ���                       ��      ,                   � ߱        x
  $    L
  ���                       Ȃ      ,                   � ߱        <  $  �  �
  ���                       Ԃ      ,                   � ߱          L      �                        ��        0         �  �                  @j�    ,  `�     �     �  �
      $  �  x  ���                       ��      ,                   � ߱        �  $  �  �  ���                       �      ,                   � ߱            4   ����8�      $  �  8  ���                       t�      +    ��             � ߱          �      8  �                      ��        0         �  �                  �j�    ,  �     �     �  d      $  �    ���                       ��      ,                   � ߱        �  $  �  d  ���                       ��      ,                   � ߱            4   �����  �  $  �  �  ���                        �      ,    ,�             � ߱            $  �  $  ���                       8�      ,    D�             � ߱        P�      ,    \�         p�      ,    |�         ��      +                   � ߱        �  $  �  P  ���                         A  �       # <   ��         0                                             ��                 �  x           ��           ��         �            X   h                �  L<      �  l      ��                  �  �  �              v�                       �  �      @  �       ��                            7   ����    %      ��                     �            �                  6   �       %    ��                 �            �                                                        ��                 X  L           ̄           Ԅ                      ,   <        O   ����  e�          O   ����  R�          O   ����  ��        $  �  �  ���                       ܄      +                   � ߱        d  $  �  8  ���                       �      +                   � ߱        �  $  �  �  ���                       0�      +                   � ߱          $  �  �  ���                       X�      ,                   � ߱        l  $  �  @  ���                       d�      , 
       
           � ߱        �  $  �  �  ���                       p�      ,                   � ߱          $  �  �  ���                       |�      , 	       	           � ߱        �    �  8  �      0�      4   ����0�  	              0                      ��             
     �  �                  \1�                       �  H  
  @      �                         ��        0    
     �  �                  �1�    ,  І     D     �  �      $  �  l  ���                       P�      ,                   � ߱        �  $  �  �  ���                       ��      ,                   � ߱            4   ������        �    ,      �      4   �����      O   �  ��	 ��          $  �  p  ���                       �      , 	       	           � ߱             �  �  �      ,�      4   ����,�      $  �  �  ���                       L�      , 	       	           � ߱        d    �  <  L      `�      4   ����`�      O   �  �� ��      �  $  �  �  ���                       p�      ,                   � ߱        �  p   �  |�  �      �    T     ��                d                      ��                  �  �                  ���                       �  �  �  $  �  �  ���                       ��      , 
       
           � ߱            $  �  �  ���                       ��      ,                   � ߱        �  �     ��                �                      ��                  �  �                   ��                       �  $  �  $  �  �  ���                       ��      , 
       
           � ߱        P  $  �  $  ���                       ć      ,                   � ߱            $  �  |  ���                       Ї      ,                   � ߱          �     ܇        �  �  P      �      4   �����                `                      ��                  �  �                  ���                       �  �  �  $  �  �  ���                       �      , 
       
           � ߱            $  �  �  ���                       �      ,                   � ߱        x        D�        �  <  �      P�      4   ����P�                �                      ��                  �  �                  Dk�                       �  L     $  �  �  ���                       x�      , 
       
           � ߱            $  �  L  ���                       ��      ,                   � ߱            �     ��        �  �         ��      4   ������                0                      ��                  �  �                  �k�                       �  �  �  $  �  \  ���                       Ĉ      , 
       
           � ߱            $  �  �  ���                       Ј      ,                   � ߱        H!    �  �  x      ��      4   ������                �                      ��                  �  �                  tl�                       �    4   9   �     �          $�         8�          D�         X�          d�         x�          ��         ��          ��             � ߱        `   $  �  �  ���                             �  |   �       ��      4   ������      $  �  �   ���                       ��          ̉             � ߱        ��      ,    ��         $�      ,    0�         X�      +                   � ߱            $  �  �   ���                                   ,  �!                                             + ^�9          5  |7  t��!             �     ��     �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         t   ,D  T  d  t  hx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������		(	8	H	X	h	x	�	�	�	�	�	�	�	�	
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
(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������   t   ,D  T  d  t  hx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������			(	8	H	X	h	x	�	�	�	�	�	�	�	�
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
�(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������(8HXhx��������  �T� $   / ? O _ o  � � � � � � � �                                                                                                                                                                                                                                                                                                            �     + ,   ��                             ��                             ��                              ��                           ��                             ��                            ����                                #      =   P         =   �                     �           �   l       ��                      �               Lo�                    O   ����    e�          O   ����    R�          O   ����    ��      �   /     �                                 3   ������      /                                      3   ������    ��                            ����                                            H          �   l       ��                   �  �               �r�                    O   ����    e�          O   ����    R�          O   ����    ��      i$   -                   �                X      \          ,        ��                  &  <  D              ���                0
     &  �       �  �  �                                7   ����          ��               ��    �            $                  6   &        T   ��         H  ��    �            $                                                        ��                 �  �                                   @            p   �          �      8          �                                                                                                                                                                                                              J   &          �    ��                                                           ��                      �              O   ����  e�          O   ����  R�          O   ����  ��      �    (  x  �      �      4   �����      O   (  �� ��      �    )  �  �      T�      4   ����T�      O   )  �� ��      <  $  +    ���                       ��      .                   � ߱        �  $  ,  h  ���                       ��      .                   � ߱        �  $  -  �  ���                       ȋ      .                   � ߱        p    /          ԋ      4   ����ԋ      $  /  D  ���                       �      .                   � ߱        �    0  �  �      �      4   �����      $  0  �  ���                       P�      .                   � ߱        x    1           \�      4   ����\�      $  1  L  ���                       ��      .                   � ߱        �    2  �  �      ��      4   ������      $  2  �  ���                       ��      .                   � ߱        �    3    (      ��      4   ������      $  3  T  ���                       4�      .                   � ߱        	    4  �  �      @�      4   ����@�      $  4  �  ���                       x�      .                   � ߱            /  6  0	     @	  ��                      3   ������  p	        `	                      3   ������  �	        �	                      3   ������  �	        �	                      3   �����   
        �	                      3   ������             
                      3   �����  0    R  L
  �
      �      4   �����                �
                      ��                  R  V                  :�                       R  \
    /  S         D�                      3   ����0�  D        4                      3   ����P�  t        d                      3   ����\�  �        �                      3   ����h�  �        �                      3   ����t�            �                      3   ������      /  T  0     @  ��                      3   ������  p        `                      3   ������  �        �                      3   ������  �        �                      3   ����Ď           �                      3   ����Ў                                   3   ����܎      W  L  �      �      4   �����                �                      ��                  W  Z                  �:�                       W  \      /  X         �                      3   �����  D        4                      3   ����(�  t        d                      3   ����4�  �        �                      3   ����@�  �        �                      3   ����L�            �                      3   ����X�  �    b     �      d�      4   ����d�                                        ��                  b  v                  h;�                       b  0  T    z  �  D      ��      4   ������                                        ��                  z  �                  �;�                       z  �  �    �  p  �      ��      4   ������                                        ��                  �  �                  p<�                       �  �        �    �      ď      4   ����ď                �                      ��                  �  �                  �<�                       �  (  �  $  �  �  ���                       �      -                   � ߱            /   �  (     8                          3   �����  h        X                      3   ����4�            �                      3   ����@�              .                                                -  �          x  �    �                                                                                                                       	     0   @   P   `   p   �      	     0   @   P   `   p   �          - .   ��                             ��                            ����                                            �           �   l       ��                  �  �  �               �=�                    O   ����    e�          O   ����    R�          O   ����    ��      i$   /                   �               �  �         L�      4   ����L�      O   �  ��  ��  ��  d    �  <  L      ��      4   ������      O   �  ��  ��  ܐ  �    �  �  �      �      4   �����      O   �  ��  ��  �        �  �  �      �      4   �����      O   �  ��  ��  4�             / 	 �          �  �  $ �                                                                                                                              
 $   4   D   T   d   t   �   �      
 $   4   D   T   d   t   �   �              /     ��                            ����                                                      �   l       ��                 �  9  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��      o&   0    �              �          i$   0                   �          \  $ �  0  ���                       @�      0                   � ߱        �  $ �  �  ���                       l�      0                   � ߱              �  �  L     ��      4   ������                \                      ��                  �  �                  d�                       �  �      $  �  �  ���                       ��      0 	       	           � ߱                      �                      ��                    4                  ��                         �    �        l                      ��        0                             X�    0  ��            0      $    �  ���                        �      0                   � ߱        \  $    0  ���                       P�      0                   � ߱            4   ����x�  �  $   �  ���                       ��     
 0                   � ߱            $    �  ���                       ��      0                   � ߱        �  $    H  ���                       ̓     
 0                   � ߱          �      H  �                      ��        0           3                  d��    0  `�              t      $      ���                       ��      0                   � ߱        �  $    t  ���                       �      0                   � ߱            4   ����8�    $   �  ���                       t�     
 0                   � ߱        L      $  4      ��      4   ������      O     �� ��      �      h  x      Ȕ      4   ����Ȕ      O     �� ��      �  /    �     �  ��                      3   ������  �        �                      3   �����         
     ,                  3   �����      $     X  ���                               
 0 
       
           � ߱        �    "  �  �       �      4   ���� �      O   "  �� ��      �
  p   $  @�  �  D
  ,  �	  `	     T�                p	                      ��                  %  (                  (��                       %  �  �	  $   &  �	  ���                       ��  @         l�              � ߱            �   '  ԕ          �	     <�      $  )  
  ���                       ��  @         T�              � ߱            $  +  p
  ���                       �  @         �              � ߱        �
    -  H�     T�  �
  �   0  `�      O   2  �� ��                 0 
 �          �  �  ( �                                                                                   
                                         
              (   8   H   X   h   x   �   �   �       (   8   H   X   h   x   �   �   �        �  �  0     ��                            ����                                                      �   l       ��                 ?  �  �               d��                    O   ����    e�          O   ����    R�          O   ����    ��      o&   1    �              �          i$   1                   �              p   S  l�     8  �  �  0     x�      /   T  \     l                          3   ������            �                      3   ������    �     ��      /   U  �     �                          3   ������                                  3   ����ؗ  �  �     �                �                      ��                  W  \                  ù                       W  (    /  X  �     �                          3   �����                                   3   �����        Y  ,  <  T  �      4   �����      O   Y  ��  ��  4�      	  Z  �                                    �  3   ����8�      3   ����D�    �     H�  T�  `�  l�  x�      	   d  �                                          3   ������  �       ��      	  f  P                                    `  3   ������  p  3   ������      3   ������      �     ܘ                                      ��                  j  ~                  Ĺ                       j  �  P    n  (  8      �      4   �����      O   n  ��  ��  �  �    q  l  |      �      4   �����      O   q  ��  ��  4�       t  �  ,      @�      4   ����@�                <                      ��                  t  z                  xĹ                       t  �  �  �  x  `�      T         
   t  �                  3   ����l�      $   x  �  ���                               
 1 	       	           � ߱              y  �        x�      4   ����x�      O   y  ��  ��  ��      O   }  ��  ��  ��        �  T  d      ��      4   ������      /   �  �     �                          3   ����ؙ  �        �                      3   ������            �                      3   ���� �             1 	 �          �  �  $ � 4                                                                                                              
             
 $   4   D   T   d   t   �   �      
 $   4   D   T   d   t   �   �          �   1     ��                              ��                          ����                                            �           �   l       ��                 �  �  �               ǹ                    O   ����    e�          O   ����    R�          O   ����    ��      i$   2                   �          �    �  �   t      �      4   �����                                        ��                  �  �                  ��                       �          �  �        ,�      4   ����,�                                        ��                  �  �                  4ֹ                       �  �             2 
 @          �    ( � `                                                                                                                                           (   8   H   X   h   x   �   �   �       (   8   H   X   h   x   �   �   �              2     ��                            ����                                ��          ^  �   �d  g  P                      
 �                                                                 �   X     3  P     �	�(                                    
 �                                                                �   X     z         �(                                    
 �                                                                �   X     3  P       �(                                      �                                                                                                                                                            �          9  d   �\  B  P                      
 �                                                                 �   X     K       )                                    
 �                                                                �   X     Q  x     s	)                                    
 �                                                                �   X     X  
       )                                      �                                                                                                                                                  6 �/5          �  �
   ��    �                      
 �                                                                    X     (       ��(                                    
 �                                                                   X     -  2     �	#)                                    
 �                                                                   X     (         6)                                    
 �                                                                   X     3  P       ?)                                      �                                                                                                                                                                               L
   d d     �   ��J�
J  � �                                                                                                                       d     D                                                                 H  � � �                                 9          �          \  4!w tM                                 �                 O)                @      \  �'��p                                 �                 w)                @      h  � w  M                                                        �     �     �)               H  F� ��                                ^          �          P   u�Q                                                           �)  G     x  uTX                                                        �     �             "  
       �  �       H  � z�/5                                �          �            D                                                                    TXS appSrvUtils tGrupoAccesos Tabla de Reportes Task-No Llave-C Campo-D Campo-F Campo-I Campo-C Llave-I Llave-F Llave-D Campo-L tUsuarioGrupos tUsuarios ASSIGNFOCUSEDWIDGET ASSIGNWIDGETVALUE ASSIGNWIDGETVALUELIST BLANKWIDGET CLEARWIDGET DISABLERADIOBUTTON DISABLEWIDGET ENABLERADIOBUTTON ENABLEWIDGET FORMATTEDWIDGETVALUE FORMATTEDWIDGETVALUELIST HIDEWIDGET HIGHLIGHTWIDGET RESETWIDGETVALUE TOGGLEWIDGET VIEWWIDGET WIDGETHANDLE WIDGETLONGCHARVALUE WIDGETISBLANK WIDGETISFOCUSED WIDGETISMODIFIED WIDGETISTRUE WIDGETVALUE WIDGETVALUELIST x-filtro-menu <Todos> x-tGrupoAccesos t-perfil-user t-UsuarioGrupos opcionesMenu cmodulo dmodulo cmenu dmenu opcionesMenuAll cuser duser dtipo wWin h_pure4gltv BUTTON-1 BUTTON-6 COMBO-BOX-modulo Item 1 TOGGLE-usuarios-activos BROWSE-2 GRUPO DE ACCESOS DEL MODULO - PERFIL X(8) X(50) X(80) BROWSE-6 Usuarios X(15) X(120) X(10) BROWSE-7 GRUPOS DEL USUARIO X(6) fMain yes/no X(256) GUI Perfil de Usuario DISABLEPAGESINFOLDER ENABLEPAGESINFOLDER GETCALLERPROCEDURE GETCALLERWINDOW GETCONTAINERMODE GETCONTAINERTARGET GETCONTAINERTARGETEVENTS GETCURRENTPAGE GETDISABLEDADDMODETABS GETDYNAMICSDOPROCEDURE GETFILTERSOURCE GETMULTIINSTANCEACTIVATED GETMULTIINSTANCESUPPORTED GETNAVIGATIONSOURCE GETNAVIGATIONSOURCEEVENTS GETNAVIGATIONTARGET GETOUTMESSAGETARGET GETPAGENTARGET GETPAGESOURCE GETPRIMARYSDOTARGET GETREENABLEDATALINKS GETRUNDOOPTIONS GETRUNMULTIPLE GETSAVEDCONTAINERMODE GETSDOFOREIGNFIELDS GETTOPONLY GETUPDATESOURCE GETUPDATETARGET GETWAITFOROBJECT GETWINDOWTITLEVIEWER GETSTATUSAREA PAGENTARGETS SETCALLEROBJECT SETCALLERPROCEDURE SETCALLERWINDOW SETCONTAINERMODE SETCONTAINERTARGET SETCURRENTPAGE SETDISABLEDADDMODETABS SETDYNAMICSDOPROCEDURE SETFILTERSOURCE SETINMESSAGETARGET SETMULTIINSTANCEACTIVATED SETMULTIINSTANCESUPPORTED SETNAVIGATIONSOURCE SETNAVIGATIONSOURCEEVENTS SETNAVIGATIONTARGET SETOUTMESSAGETARGET SETPAGENTARGET SETPAGESOURCE SETPRIMARYSDOTARGET SETREENABLEDATALINKS SETROUTERTARGET SETRUNDOOPTIONS SETRUNMULTIPLE SETSAVEDCONTAINERMODE SETSDOFOREIGNFIELDS SETTOPONLY SETUPDATESOURCE SETUPDATETARGET SETWAITFOROBJECT SETWINDOWTITLEVIEWER GETOBJECTTYPE SETSTATUSAREA GETALLFIELDHANDLES GETALLFIELDNAMES GETCOL GETDEFAULTLAYOUT GETDISABLEONINIT GETENABLEDOBJFLDS GETENABLEDOBJHDLS GETHEIGHT GETHIDEONINIT GETLAYOUTOPTIONS GETLAYOUTVARIABLE GETOBJECTENABLED GETOBJECTLAYOUT GETROW GETWIDTH GETRESIZEHORIZONTAL GETRESIZEVERTICAL SETALLFIELDHANDLES SETALLFIELDNAMES SETDEFAULTLAYOUT SETDISABLEONINIT SETHIDEONINIT SETLAYOUTOPTIONS SETOBJECTLAYOUT SETRESIZEHORIZONTAL SETRESIZEVERTICAL GETOBJECTTRANSLATED GETOBJECTSECURED CREATEUIEVENTS GETAPPSERVICE GETASBOUND GETASDIVISION GETASHANDLE GETASHASSTARTED GETASINFO GETASINITIALIZEONRUN GETASUSEPROMPT GETSERVERFILENAME GETSERVEROPERATINGMODE RUNSERVERPROCEDURE SETAPPSERVICE SETASDIVISION SETASHANDLE SETASINFO SETASINITIALIZEONRUN SETASUSEPROMPT SETSERVERFILENAME SETSERVEROPERATINGMODE gshAstraAppserver gshSessionManager gshRIManager gshSecurityManager gshProfileManager gshRepositoryManager gshTranslationManager gshWebManager gscSessionId gsdSessionObj gshFinManager gshGenManager gshAgnManager gsdTempUniqueID gsdUserObj gsdRenderTypeObj gsdSessionScopeObj ghProp ghADMProps ghADMPropsBuf glADMLoadFromRepos glADMOk ANYMESSAGE ASSIGNLINKPROPERTY FETCHMESSAGES GETCHILDDATAKEY GETCONTAINERHANDLE GETCONTAINERHIDDEN GETCONTAINERSOURCE GETCONTAINERSOURCEEVENTS GETCONTAINERTYPE GETDATALINKSENABLED GETDATASOURCE GETDATASOURCEEVENTS GETDATASOURCENAMES GETDATATARGET GETDATATARGETEVENTS GETDBAWARE GETDESIGNDATAOBJECT GETDYNAMICOBJECT GETINSTANCEPROPERTIES GETLOGICALOBJECTNAME GETLOGICALVERSION GETOBJECTHIDDEN GETOBJECTINITIALIZED GETOBJECTNAME GETOBJECTPAGE GETOBJECTPARENT GETOBJECTVERSION GETOBJECTVERSIONNUMBER GETPARENTDATAKEY GETPASSTHROUGHLINKS GETPHYSICALOBJECTNAME GETPHYSICALVERSION GETPROPERTYDIALOG GETQUERYOBJECT GETRUNATTRIBUTE GETSUPPORTEDLINKS GETTRANSLATABLEPROPERTIES GETUIBMODE GETUSERPROPERTY INSTANCEPROPERTYLIST LINKHANDLES LINKPROPERTY MAPPEDENTRY MESSAGENUMBER PROPERTYTYPE REVIEWMESSAGES SETCHILDDATAKEY SETCONTAINERHIDDEN SETCONTAINERSOURCE SETCONTAINERSOURCEEVENTS SETDATALINKSENABLED SETDATASOURCE SETDATASOURCEEVENTS SETDATASOURCENAMES SETDATATARGET SETDATATARGETEVENTS SETDBAWARE SETDESIGNDATAOBJECT SETDYNAMICOBJECT SETINSTANCEPROPERTIES SETLOGICALOBJECTNAME SETLOGICALVERSION SETOBJECTNAME SETOBJECTPARENT SETOBJECTVERSION SETPARENTDATAKEY SETPASSTHROUGHLINKS SETPHYSICALOBJECTNAME SETPHYSICALVERSION SETRUNATTRIBUTE SETSUPPORTEDLINKS SETTRANSLATABLEPROPERTIES SETUIBMODE SETUSERPROPERTY SHOWMESSAGE SIGNATURE , prepareInstance ObjectName CHAR  ObjectVersion ADM2.2 ObjectType SmartWindow ContainerType WINDOW PropertyDialog adm2/support/visuald.w QueryObject LOGICAL ContainerHandle HANDLE InstanceProperties LogicalObjectName,PhysicalObjectName,DynamicObject,RunAttribute,HideOnInit,DisableOnInit,ObjectLayout SupportedLinks Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source ContainerHidden ObjectInitialized ObjectHidden ObjectsCreated HideOnInit UIBMode ContainerSource ContainerSourceEvents initializeObject,hideObject,viewObject,destroyObject,enableObject,confirmExit,confirmCancel,confirmOk,isUpdateActive DataSource DataSourceEvents dataAvailable,queryPosition,updateState,deleteComplete,fetchDataSet,confirmContinue,confirmCommit,confirmUndo,assignMaxGuess,isUpdatePending TranslatableProperties ObjectPage INT DBAware DesignDataObject DataSourceNames DataTarget DataTargetEvents CHARACTER updateState,rowObjectState,fetchBatch,LinkState LogicalObjectName PhysicalObjectName LogicalVersion PhysicalVersion DynamicObject RunAttribute ChildDataKey ParentDataKey DataLinksEnabled InactiveLinks InstanceId DECIMAL SuperProcedure SuperProcedureMode SuperProcedureHandle LayoutPosition ClassName RenderingProcedure ThinRenderingProcedure Label cType ADMProps Target WHERE Target = WIDGET-H(" ") AppService ASDivision ASHandle ASHasConnected ASHasStarted ASInfo ASInitializeOnRun ASUsePrompt BindSignature BindScope ServerOperatingMode ServerFileName ServerFirstCall NeedContext ObjectLayout LayoutOptions ObjectEnabled LayoutVariable DefaultLayout DisableOnInit EnabledObjFlds EnabledObjHdls FieldSecurity SecuredTokens AllFieldHandles AllFieldNames MinHeight MinWidth ResizeHorizontal ResizeVertical ObjectSecured ObjectTranslated PopupButtonsInFields ColorInfoBG INTEGER ColorInfoFG ColorWarnBG ColorWarnFG ColorErrorBG ColorErrorFG BGColor FGColor FieldPopupMapping CurrentPage PendingPage ContainerTarget ContainerTargetEvents exitObject,okObject,cancelObject,updateActive ContainerToolbarSource ContainerToolbarSourceEvents toolbar,okObject,cancelObject OutMessageTarget PageNTarget PageSource FilterSource UpdateSource UpdateTarget CommitSource CommitSourceEvents commitTransaction,undoTransaction CommitTarget CommitTargetEvents rowObjectState StartPage RunMultiple WaitForObject DynamicSDOProcedure adm2/dyndata.w RunDOOptions InitialPageList WindowFrameHandle Page0LayoutManager MultiInstanceSupported MultiInstanceActivated ContainerMode SavedContainerMode SdoForeignFields NavigationSource NavigationTarget PrimarySdoTarget NavigationSourceEvents fetchFirst,fetchNext,fetchPrev,fetchLast,startFilter CallerWindow CallerProcedure CallerObject DisabledAddModeTabs ReEnableDataLinks WindowTitleViewer UpdateActive InstanceNames ClientNames ContainedDataObjects ContainedAppServices DataContainer HasDbAwareObjects HasDynamicProxy HideOnClose HideChildContainersOnClose HasObjectMenu RequiredPages RemoveMenuOnHide ProcessList PageLayoutInfo PageTokens DataContainerName WidgetIDFileName ghContainer adm2/smart.p cObjectName iStart / \ . hReposBuffer hPropTable hBuffer hTable deleteProperties ADM-CLONE-PROPS pcProcName hProc START-SUPER-PROC cAppService cASDivision cServerOperatingMode adm2/appserver.p getAppService Server NONE setASDivision setAppService cFields adm2/visual.p   BROWSE-6 BUTTON-6 BUTTON-1 TOGGLE-usuarios-activos BROWSE-7 COMBO-BOX-modulo BROWSE-2 CTRL-PAGE-UP CTRL-PAGE-DOWN adm2/containr.p Add initializeDataObjects getSupportedLinks data-target data-source buildDataRequest containertoolbar-target ContainerToolbar-Target CLOSE  GENERAL c-csv-file c-xls-file d:\xpciman\opcionesMenu .xlsx iStartPage ADM-ERROR celda_br cual_celda n_cols_browse col_act t_col_br vg_col_eti_b PROCESO currentPage pure4gltv.w wineModeAutomaticwindowsSkinRoyalepicCacheCoef1labCacheCoef1tvIterationHeight17TreeStyle3FocSelNodeBgColor1UnfSelNodeBgColor8tvnodeDefaultFont1FocSelNodeFgColor15UnfSelNodeFgColor0resizeVerticalyesresizeHorizontalyesDragSourcenoneautoSortnoMSkeyScrollForcePaintyesHideOnInitnoDisableOnInitnoObjectLayout tvNodeEvent AFTER ADM-CREATE-OBJECTS x-grupos x-sec PF-G004 Aplicaciones por Usuario , PF-G003 Aplicaciones <Inexistente> OpcMnu NumOpc LenOpc H-parent H-label Nivel-Act Nivel-Ant s-seguridad s-aplic-id s-user-id OK s-codmenu s-label s-programa i MAIN PF-G002 Opciones de Men�s ADMIN NO CREAR NADA SUB-MENU LINEA - SEPARADOR                     CAJA-MARCADOR * ( ) MENU DEL USUARIO :  CARGA-GRUPOS _User ACTIVO INACTIVO CARGA-USUARIOS pParent tvpics/menumenubar.bmp addOnExpand tvRefresh CARGAR-TREEVIEW DISABLE_UI ENABLE_UI d:\xpciman\PerfilesTodosUsuarios.xlsx EXCEL-PERFIL-TODOS-LOS-USUARIOS EXITOBJECT pUser GET-USUARIO-OPCIONES-MENU INITIALIZEOBJECT pcnodeKey x-aplic-id x-icon x-expanded x-label tvpics/separator.bmp tvpics/table.bmp tvpics/menusubmenu.bmp nCust norder cSalesrep icustnum iorder cFullPath n222 n2221 node 2221 tvpics/books05.bmp selected n2222 node 2222 n22221 node 22221 tvpics/user.bmp sr= cust= order= fileName= = TVNODEADDONEXPAND ccustname cparentKey n Add a child node,MenuAddChildNode,RULE,,Hello World,MenuHelloWorld Add Salesrep,MenuAddSR,Add Customer,MenuAddCustomer Add order,MenuAddOrder Add order line,MenuAddOrderLine TVNODECREATEPOPUP pcEvent mouseX mouseY cWidgets hWidget icount k targetKe getNodeLocatedAtXY   SCREEN-VALUE SCREEN-VALUE= ? hNodeBuffer EDITOR lab 
 FILL-IN VALUE-CHANGED TVNODEDROPEND select rightClick tvNodeCreatePopup failed with the following message: MenuAddChildNode MenuAddSR MenuAddCustomer MenuAddOrder MenuAddOrderLine addToEdMsg('Menu item event fired: ' + pcEvent + ' for key ' + pcnodeKey + '
') MenuHelloWorld Hello World! Node key parent of the popup menu item: DragBegin dropOnYourself n4 cancelDrag n1 hTargetFrame getOtherWinTargetFrame DropEnd, TVNODEEVENT optn MoreCust= MoreOrder= TVNODESELECT REPO01 REPO02 REPO03 default Grupo Modulo Nombre del Modulo Usuario Nombre y Apellidos Estado Descripcion Modulo Cod.Menu Opcion del Menu Todos los accesos de todos los usuarios Enviar a Excel el Perfil Mostrar solo usuarios activos Modulos IDX01 _Userid (   �9  X   `@      % �8   ��      0         pcProp      ��      P         pcProp      ��      p         plCancel    �   ��      �         pcProcName  �   ��      �        
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
 hTarget X  ��      L        pcMessage       ��      p        pcMessage       ��      �        plEnabled             �     cType       �     6   �          �                  getObjectType   �	  �	  �	  ,          
   hReposBuffer    L        @  
   hPropTable  h        `  
   hBuffer           |  
   hTable  �  �     7             �                  adm-clone-props �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �              
   hProc             <        pcProcName  �  �  	   8     $      x                  start-super-proc    !  "  #  $  %  &  A  N  P  H  �     9                                     �  	     :                                      !  �  L	     ;                                   $  %  	  �	     <                                   c  T	  �	     =                                   u  v  �	  �	     >                                   �  �  �  �	  ,
     ?                                   �  �	  `
     @                                   �  �  �  �  �  �
        �
  
   hProc   �
        �
     c-csv-file            �
     c-xls-file  0
        A   t
                              �  �  �  �  �  �  �  �  �  �  �  �
  \     B                                   �  �  ,  �     C                                   �  �  �  �  d  �     D                                   �  �  �       E                                            �  L     F                                   &  '    �     G                                   L  N  P  Q  S  T  V  W             �     currentPage T       H   �          �                  adm-create-objects  r  t  v  w  |  }  �  �  �  �  �  T  !      H     x-grupos    p  !      h     x-sec   �  $   ,�     OpcMnu  �  $     �     NumOpc  �  $     �     LenOpc  �  !     �     H-parent       !     �     H-label    $          Nivel-Act   @  $     4     Nivel-Ant   `  !      T     s-seguridad �  !      t     s-aplic-id  �  !      �     s-user-id   �  !   	   �     OK  �  !   
   �     s-codmenu   �  !      �     s-label   !           s-programa      !      (     i   �  l  h   I   4          \                  carga-grupos    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �             	  
                %  (  )  *  +  1  2  3  4  :  ;  <  =  D  F  G  H  O  P  R  U  W  Y  Z  \  ^  `  a  c  ,  L     J               <                  carga-usuarios  o  q  r  s  v  x  {  |    �  �  �  �  �  �  �  �  �      '      �        pParent   �  	   K       �      �                  cargar-treeview �  �  �  �  �  �  �  �  �  �  T     L               H                  disable_UI  �  �  �  �    �     M               �                  enable_UI   �  �  �  �  �  �  *      �  
   hProc   �  *      �     c-csv-file      *           c-xls-file  d  `     N   �          @                  excel-perfil-todos-los-usuarios �  �  �  �  �  �  �  �           
                   �     O               �                  exitObject  -  .  0    ,          x-grupos    4  ,     ,     x-sec   P  +    ,H     OpcMnu  l  +      d     NumOpc  �  +      �     LenOpc  �  ,    �     H-parent    �  ,    �     H-label �  +      �     Nivel-Act     +      �     Nivel-Ant   $  ,          s-seguridad D  ,     8     s-aplic-id  d  ,     X     s-user-id   |  ,  	   x     OK  �  ,  
   �     s-codmenu   �  ,     �     s-label �  ,     �     s-programa      ,     �     i       +              pUser   �  \  P   P   �  �      @                  get-usuario-opciones-menu   ?  @  E  F  G  H  I  L  M  N  O  P  R  T  i  }  ~    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �    �     Q               �                  initializeObject            .           x-aplic-id  (  .           x-icon  H  .     <     x-expanded  d  .     \     x-label �  -      x     nCust   �  -      �     norder  �  -      �     cSalesrep   �  -      �     icustnum    �  -      �     iorder      -           cFullPath       -      0        pcnodeKey   �  �      R   �        l                  tvNodeAddOnExpand   &  (  )  +  ,  -  /  0  1  2  3  4  6  <  R  S  T  V  W  X  Z  b  v  z  �  �  �  �  �  �  �  �    /           nCust   8  /      0     norder  X  /      L     cSalesrep   x  /      l     icustnum    �  /      �     ccustname   �  /      �     iorder      /   	   �     cparentKey      /      �        pcnodeKey   <  <  	   S      �      (                  tvNodeCreatePopup   �  �  �  �  �  �  �  �  �  |  0      t     mouseX  �  0      �     mouseY  �  0      �     cWidgets    �  0      �  
   hWidget �  0      �     icount    0   	        targetKe        0   
   $  
   hNodeBuffer P  0      H        pcEvent     0      h        pcnodeKey   �  �     T   `  0      �                  tvNodeDropEnd   �  �  �  �  �                        "  $  %  &  '  (  )  +  ,  -  0  2  3  4  9  L  1      D     nCust   h  1      `     norder  �  1      |     cSalesrep   �  1      �     icustnum    �  1      �     iorder      1   	   �  
   hTargetFrame      1               pcEvent     1               pcnodeKey   t  h     U   0  �      \                  tvnodeEvent S  T  U  W  X  Y  Z  \  d  f  j  n  q  t  x  y  z  }  ~  �  �  �  �  2      �     nCust   �  2      �     norder    2           cSalesrep   8  2      ,     icustnum    X  2      L     ccustname   t  2      l     iorder  �  2   	   �     cparentKey      2   
   �     optn        2      �        pcnodeKey   ,        V   �  �                         tvNodeSelect    �  �  �  �  �  �  -      ! �%      �+                      @!  h   x   
   tGrupoAccesos   �          �           !         !        !        !         !        (!         0!         8!        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L ("  P!  `!  
   tUsuarioGrupos  �!         �!         �!         �!        �!         "        "        "         "          "        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L #  8"  D"  
   tUsuarios   �"         �"         �"         �"        �"        �"        �"        �"         �"         #        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L �#  #  ,#  
   t-perfil-user   �#         �#         �#         �#        �#        �#        �#        �#         �#         �#        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L �$  $  $  
   t-UsuarioGrupos �$         �$         �$         �$        �$        �$        �$        �$         �$         �$        Task-No Llave-I Llave-C Campo-D Campo-F Campo-I Campo-C Llave-F Llave-D Campo-L L%  �$  �$     opcionesMenu    ,%         4%         <%         D%         cmodulo dmodulo cmenu   dmenu       \%  l%     opcionesMenuAll �%         �%         �%         �%         �%         �%         �%         cuser   duser   cmodulo dmodulo cmenu   dmenu   dtipo   &          &  
   appSrvUtils <&       ,&     x-filtro-menu   X&  
 
    P&  
   wWin    x&  
 
    l&  
   h_pure4gltv �&  
 
    �&     COMBO-BOX-modulo    �&  
 
    �&     TOGGLE-usuarios-activos �&        �&  
   gshAstraAppserver   '        '  
   gshSessionManager   @'        0'  
   gshRIManager    h'        T'  
   gshSecurityManager  �'        |'  
   gshProfileManager   �'        �'  
   gshRepositoryManager    �'        �'  
   gshTranslationManager   (        �'  
   gshWebManager   0(         (     gscSessionId    T(        D(     gsdSessionObj   x(        h(  
   gshFinManager   �(        �(  
   gshGenManager   �(        �(  
   gshAgnManager   �(        �(     gsdTempUniqueID )        �(     gsdUserObj  ,)        )     gsdRenderTypeObj    T)        @)     gsdSessionScopeObj  p)  
 
    h)  
   ghProp  �)  
 
    �)  
   ghADMProps  �)  
 
    �)  
   ghADMPropsBuf   �)  
 
 	   �)     glADMLoadFromRepos  �)  
 
 
   �)     glADMOk *  
 
    *  
   ghContainer 8*  
 
    ,*     cObjectName T*  
 
    L*     iStart  t*  
 
    h*     cAppService �*  
 
    �*     cASDivision �*  
 
    �*     cServerOperatingMode    �*  
 
    �*     cFields �*  
 
    �*     iStartPage  +  
 
  � +  
   celda_br    <+  
 
    0+  
   cual_celda  `+  
 
    P+     n_cols_browse   |+  
 
    t+     col_act �+  
 
    �+     t_col_br        
 
    �+     vg_col_eti_b    �+    \  �+  tGrupoAccesos    ,    \  �+  tUsuarioGrupos  ,    \  ,  tUsuarios   <,    B  ,,  x-tGrupoAccesos \,    \  L,  t-perfil-user   |,    \  l,  t-UsuarioGrupos �,    L  �,  opcionesMenu    �,  	 	 L  �,  opcionesMenuAll �,   "    �,  PF-G004 �,   #    �,  PF-G003 -   %    �,  PF-G002      &    -  _User            C   �  �  �                �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  	  	  	  	  	  	  
	  	  	  	  	  	  	  	  	  	  	  	  	  	  	  	  	  �	  
  
  	
  

  
  
  
  
  
  
  
  
  
  
  
  
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
  &  2  3  6  7  8  9  ;  <  >  ?  @  A  B  C  D  E  G  H  I  J  K  L  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  \  ]  ^  _  `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  �  �  )  *  3  4  8  9  :  <  ?  I  e  w  �  �  �  �  W  o  p  �  �  �  �  �  �  �  �  �  �  �    �  �  �  �            #  <  =  >  @  H  N  T  W  \  `  a  b  d  h  �  �  /  0  p  }  �  �  �  �  �  �        %  *  1  5  9  :  ;  <  >  @  K  Y  Z  [  ]  _  a      H� $ C:\Progress\OpenEdge\src\adm2\windowmn.i \1  f!  C:\Progress\OpenEdge\src\adm2\containr.i �1  � # %C:\Progress\OpenEdge\src\adm2\custom\containrcustom.i    �1  ��  C:\Progress\OpenEdge\src\adm2\visual.i   2  # " %C:\Progress\OpenEdge\src\adm2\custom\visualcustom.i  <2  �<  C:\Progress\OpenEdge\src\adm2\appserver.i    |2  �� ! %C:\Progress\OpenEdge\src\adm2\custom\appservercustom.i   �2  I�  C:\Progress\OpenEdge\src\adm2\smart.i    �2  Ds   C:\Progress\OpenEdge\gui\fn  ,3  tw  %C:\Progress\OpenEdge\src\adm2\custom\smartcustom.i   T3  Q.  C:\Progress\OpenEdge\gui\set �3  ��  C:\Progress\OpenEdge\src\adm2\cntnprop.i �3  ��  %C:\Progress\OpenEdge\src\adm2\custom\cntnpropcustom.i    �3  P  %C:\Progress\OpenEdge\src\adm2\custom\cntnprtocustom.i    44  F>  C:\Progress\OpenEdge\src\adm2\visprop.i  x4  �I  %C:\Progress\OpenEdge\src\adm2\custom\vispropcustom.i �4  ��  %C:\Progress\OpenEdge\src\adm2\custom\visprtocustom.i �4  �l 
 C:\Progress\OpenEdge\src\adm2\appsprop.i ,5  ɏ  %C:\Progress\OpenEdge\src\adm2\custom\appspropcustom.i    `5  V  %C:\Progress\OpenEdge\src\adm2\custom\appsprtocustom.i    �5  i$  C:\Progress\OpenEdge\src\adm2\smrtprop.i �5  �j  C:\Progress\OpenEdge\gui\get 6  �  %C:\Progress\OpenEdge\src\adm2\custom\smrtpropcustom.i    D6  ��  %C:\Progress\OpenEdge\src\adm2\custom\smrtprtocustom.i    �6  ��  C:\Progress\OpenEdge\src\adm2\smrtprto.i �6  Su  C:\Progress\OpenEdge\src\adm2\globals.i   7  M�  %C:\Progress\OpenEdge\src\adm2\custom\globalscustom.i 47  )a  %C:\Progress\OpenEdge\src\adm2\custom\smartdefscustom.i   t7  �  C:\Progress\OpenEdge\src\adm2\appsprto.i �7  ��  %C:\Progress\OpenEdge\src\adm2\custom\appserverdefscustom.i   �7  �X 	 C:\Progress\OpenEdge\src\adm2\visprto.i  48  !�  %C:\Progress\OpenEdge\src\adm2\custom\visualdefscustom.i  h8  n�  C:\Progress\OpenEdge\src\adm2\cntnprto.i �8  ;  %C:\Progress\OpenEdge\src\adm2\custom\containrdefscustom.i    �8  ~�  C:\Progress\OpenEdge\src\adm2\widgetprto.i   (9  e�  !C:\Progress\OpenEdge\gui\adecomm\appserv.i   `9  ��    &D:\xpciman\progress\Menu-ERP-Progress\perfiles-de-usuarios-erp-progress.w          C      �9       $    :  4        :  �         :     �     0:  �   �     @:     �     P:  �   �     `:     m  #   p:  �   W     �:     U      �:  �   N     �:     L      �:  �   K     �:     I      �:  r   -     �:  n        �:     �  "    ;  i   �     ;     �      ;  P   }     0;  �   t     @;       !   P;  �        `;     �     p;  �   �     �;     �     �;  �   �     �;     �     �;  g   �     �;     u     �;  O   ]     �;  �   �     �;     �       <  �   �     <     ]      <  �   R     0<     0     @<  �   /     P<          `<  �        p<     �     �<  �   �     �<     �     �<  �   �     �<     �     �<  �   �     �<     o     �<  }   c     �<     A      =     �     =     w      =     (     0=  7   �     @=  �   �     P=  O   �     `=     �     p=     w     �=  �   /     �=  �   &     �=  O        �=          �=     �
     �=  �   �
     �=  x   �
  
   �=  M   w
      >     f
     >     
      >  a   
  
   0>  �  �	     @>     �	     P>  �  �	     `>  O   �	     p>     q	     �>     #	     �>  �   M     �>          �>     t     �>  x   n     �>     U     �>     �     �>     �      ?     �     ?     �      ?  Q   �  
   0?     A     @?       
   P?     �     `?     �  
   p?  f   �     �?     Q  	   �?  "        �?     �     �?     �     �?  Z   �     �?     �     �?     P     �?     <      @     "     @     �      @  3   �       0@     L      @@     !       P@           