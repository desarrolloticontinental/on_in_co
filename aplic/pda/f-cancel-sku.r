	��VL'e�  8��              +                                ͊  04F0001Futf-8 MAIN D:\newsie\on_in_co\aplic\pda\f-cancel-sku.w,, PROCEDURE state-changed,,INPUT p-issuer-hdl HANDLE,INPUT p-state CHARACTER PROCEDURE send-records,, PROCEDURE enable_UI,, PROCEDURE disable_UI,, PROCEDURE adm-row-available,, PROCEDURE adm-create-objects,, PROCEDURE view-page,,INPUT p-page# INTEGER PROCEDURE select-page,,INPUT p-page# INTEGER PROCEDURE init-pages,,INPUT p-page-list CHARACTER PROCEDURE init-object,,INPUT p-proc-name CHARACTER,INPUT p-parent-hdl HANDLE,INPUT p-attr-list CHARACTER,OUTPUT p-proc-hdl HANDLE PROCEDURE delete-page,,INPUT p-page# INTEGER PROCEDURE adm-change-page,, PROCEDURE set-position,,INPUT p-row DECIMAL,INPUT p-col DECIMAL PROCEDURE set-attribute-list,,INPUT p-attr-list CHARACTER PROCEDURE notify,,INPUT p-method CHARACTER PROCEDURE new-state,,INPUT p-state CHARACTER PROCEDURE get-attribute-list,,OUTPUT p-attr-list CHARACTER PROCEDURE get-attribute,,INPUT p-attr-name CHARACTER PROCEDURE dispatch,,INPUT p-method-name CHARACTER PROCEDURE adm-view,, PROCEDURE adm-UIB-mode,, PROCEDURE adm-show-errors,, PROCEDURE adm-initialize,, PROCEDURE adm-hide,, PROCEDURE adm-exit,, PROCEDURE adm-enable,, PROCEDURE adm-edit-attribute-list,, PROCEDURE adm-disable,, PROCEDURE adm-destroy,, PROCEDURE adm-apply-entry,,       �              �              �y  �  �              �              @    +   `+  `     �,  `      .  P     p0  x     �1        4  x     �5  `     �6  `     @8  �  	   (;  `  
   �<  `     �=  X     @@  ,     lB  �      E  ,     LG  p     �I  ,     �K  �     �S  `     U        W  �     �Z       ]        _       4a  �      (b  �
     �l  t     Pn  `     �o  �      �p  �             \r  �  ? Ht  E  iSO8859-1                                                                        �   0    l                                      �                   X                         L   �E                           �      �                                                         PROGRESS                         �              X                                                                                                     
             �  
    
                  �                                                                                                                     
                �                                               �          �      � �            
                                           NO-LOCK                                             	     0   @   P   `   p   �      	     0   @   P   `   p   �    ��                                               3           ����                            undefined                                                               �           �   l                             �����               �P�                    O   ����    e�          O   ����    R�          O   ����    ��      �      2          �    w   �   `      8       4   ����8                 p                      ��                  w   {                    ��                       w   �   �  	  x   �                                        3   ����P       O   z   ��  ��  \   p      
                    � ߱        $  $  �  �  ���                       �  /	  �  P     `  �                       3   �����             �                      3   �����   8    �  �  (      �       4   �����                 8                      ��                  �  �                  A�                       �  �  t  /  �  d                               3   �����   �  $  �  �  ���                       �      
                      � ߱            /  �  �       (                      3   ����         
   (                      3   ����4    $   �  d  ���                       P  @         <              � ߱        adm-apply-entry     �                                                           l                     adm-destroy �  �                                                           |                     adm-disable   d                            �                              �                     adm-edit-attribute-list p  �                            8                              �                     adm-enable  �  @                            �                              �  
                   adm-exit    L  �                            8                              �                     adm-hide    �                                                             �                     adm-initialize    x                                                           �                     adm-show-errors �  �              L     	     �                          �                       adm-UIB-mode    �  P                      
                                                          adm-view    `  �                                                           %                     dispatch    �  $	  �           �                                      F                     get-attribute   0	  �	  �           �          �                          �  [                     get-attribute-list  �	  �	  �                     t                          p  u                     new-state   
  h
  �           �          �                          �  �  	                   notify  t
  �
  �           �    	      0                          ,  �                     set-attribute-list  �
  4  �           �    
      �                          �  �                     set-position    H  �  �           �          �                          �  �                     |  /   �  <     L                          3   �����            l                      3   �����  $     �  �                                     adm-change-page �  �                                                                                delete-page �    �           x          �                          �  -                     init-object (  �  �           �          �                          �  ]                     init-pages  �  �  �           x          �                          �  u  
                   select-page �  T  �           x          �                          �  �                     view-page   `  �  �           x          �                          �  �  	                       g   �  <          "�                                      �  �      ��                  �  �  �              �~�                    O   ����    e�          O   ����    R�          O   ����    ��          /  �  0     @  �                      3   �����            `                      3   �����    ��                              ��        3                   ����                                        P                    p                      g                               adm-create-objects  �  ,                            �                               �                     adm-row-available   @  �              8	          t
                          p
  �                     disable_UI  �                                                            �  
                   enable_UI     t                                                           
  	                   send-records    �  �                            �                                                    state-changed   �  H  �                     x                          t  .                      ��  � �NO-LOCK        �       %               %               �     }        �� :   J   %               
�             �G%     get-attribute   
"    
   %      TYPE        �  �   
 �%      adm/objects/broker.p  �
"   
   %     set-broker-owner �!
"    
   
�    �     }        �G @   ,            �     �     � P    p     \     H     4               � #     � +   �� 7   �� >   !� C   �� C   �� C   �((       
"   
 �
%   
           � E    �     
"   
   � C   �� F   �� C   !� C     � [   �(`  � ( 0       �     }        �G� E    �G    �     }        �G%              � g   � d <    P      <   � j     T   %              �     }        �G� j   �G� j   �T   %              �     }        �G� j   �G%     broker-apply-entry 
"    
   
�    %     broker-destroy  
"    
   
�    %     dispatch  �%     disable-fields %     set-attribute-list % 
    ENABLED=no %               %      adm/support/contnrd.w �
�    %               % 	    enable_UI �
�    %     set-attribute-list %     ENABLED=yes %               %      notify  %      exit    %               %     broker-hide 
"    
   
�    %     broker-initialize !
"    
   
�        %              %                   "      %                  "      �     }        ��     }        ��    }        �"      %               %     broker-UIB-mode 
"    
   
�    %     broker-view 
"    
   
�    %     broker-dispatch 
"    
   
�    "          �  � <  	 �% 	    ADM-ERROR �%      broker-get-attribute  �
"    
   
�    "      �  %$     broker-get-attribute-list �
"    
   
�    o%   o           "     %               %     broker-new-state �!
"    
   
�    "      %               %     broker-notify   
"    
   
�    " 	         �  � <  	 �% 	    ADM-ERROR �%               %$     broker-set-attribute-list �
"    
   
�    " 
     %               � 
"   
 !
"   
 �    �        h	     � �        "    �%               
"   
 �0        �     }        ��        �	    �%                  "    �%               
"   
 �0        �     }        ��        D
    �%              
"   
 �    �        �
     � �  
  
"   
   
�        �
    �     "    �%               
"   
 !
"   
 �L    0        �     }        ��            �%              �        $         "    �%               
"   
 !
"   
 �L    0        �     }        ��        �    �%              �        �     ( (       "      %                   "      %              %              ( (       "      %                   "      %              %              
"   
 !�             "      
"   
 ��        D     "      %               %     set-attribute-list      � �  ! tr     
"   
 �%               %     broker-change-page 
"    
   
�    %     broker-delete-page 
"    
   
�    "      %     broker-init-object 
"    
   
�    "      
"   
   "      
"   
   %               %     broker-init-pages !
"    
   
�    "      %     broker-select-page 
"    
   
�    "      %     broker-view-page �!
"    
   
�    "      %     Procesa-Botones 
"     
   � �  
   %     check-modified  
�    %      check   "      %               %     get-attribute   %     FIELDS-ENABLED (       �  � M     %              %               %     get-link-handle 
"    
   
�    %     RECORD-SOURCE �"          "    �� E    !%               
�( T    %              "    �    �     "      %              � _     �     }        �A� q  (   %      
       � �     
"   
   �        �    �A� �     %     get-attribute   %     Key-Name  ��      "  
  �� E    !%                  "  
  �%              %     send-key  �
"   
   "  
    "          "    �%              %     set-attribute-list �  ��� �   tr"     � 
"   
 !%     dispatch  �
�    %     display-fields %      notify  
�    %     row-available ��     }        �
�                    �           �   l       ��                  �  �  �               `o�                    O   ����    e�          O   ����    R�          O   ����    ��          /	  �  �      �   �                      3   �����         
                         3   �����    ��                            ����                                            �           �   l       ��                  $  -  �               �q�                    O   ����    e�          O   ����    R�          O   ����    ��          /	  +  �      �                         3   �����         
                         3   ����    ��                            ����                                            �           �   l       ��                  7  F  �               ly�                    O   ����    e�          O   ����    R�          O   ����    ��          �               � ߱           i   ?  �    �                         l  /   @  ,     <                          3   ����            \                      3   ����0  �  /   C  �     �                          3   ����L            �                      3   ����l      O   E  ��  ��  �    ��                              ��        3                   ����                                            �           �   l       ��                  P  ]  �               \z�                    O   ����    e�          O   ����    R�          O   ����    ��         /   W  �      �                           3   �����         
                         3   �����      O   \  ��  ��  �    ��                            ����                                            �           �   l       ��                  g  {  �               8��                    O   ����    e�          O   ����    R�          O   ����    ��          �               � ߱           h  q  �    �                        <  /	  t  ,         �                      3   �����  �  /   x  h     x                          3   �����            �                      3   ����      O   z  ��  ��  0    ��                              ��        3                   ����                                            �           �   l       ��                  �  �  �               (��                    O   ����    e�          O   ����    R�          O   ����    ��         /   �  �      �                           3   ����D                                  3   ����X      O   �  ��  ��  l    ��                            ����                                            �           �   l       ��                  �  �  �               t��                    O   ����    e�          O   ����    R�          O   ����    ��          /	  �  �      �   �                      3   �����         
                         3   �����    ��                            ����                                            �           �   l       ��                  �  �  �               4��                    O   ����    e�          O   ����    R�          O   ����    ��          /	  �  �      �   �                      3   �����         
                         3   �����    ��                            ����                                                       �   l       ��                 �  �  �               p��                    O   ����    e�          O   ����    R�          O   ����    ��        0      �  �                      ��        0         �  �                  ܢ�      `     4     �  �       $  �  \  ���                       �                         � ߱        �  $  �  �  ���                                                � ߱            4   ����8      	   �  $                                          3   ����t      O   �  ��  ��  �               �          �  �    �                                             ��                            ����                                            �           �   l       ��                  �  �  �               ��                    O   ����    e�          O   ����    R�          O   ����    ��          /	  �  �      �   �                      3   �����         
                         3   �����    ��                            ����                                            �           �   l       ��                  �  �  �               `ݥ                    O   ����    e�          O   ����    R�          O   ����    ��          /	  �  �      �   �                      3   �����         
                         3   �����    ��                            ����                                            �           �   l       ��                      �               �ݥ                    O   ����    e�          O   ����    R�          O   ����    ��      .                      �          x  /	                                  3   ����  H     
   8                      3   ����,            h                      3   ����4          �  �      @      4   ����@      O     ��  ��  X                                �                                             ��                            ����                                            �           �   l       ��                  #  0  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      O                      �          x  /	  ,         �                      3   ����p  H     
   8                      3   �����            h                      3   �����      O   /  ��  ��  �               �          �  �    �                                             ��                            ����                                            �           �   l       ��                  :  Q  �               �Z�                    O   ����    e�          O   ����    R�          O   ����    ��      i                      �             /	  K         �                      3   �����  H     
   8                      3   �����  x     o   h                      3   �����            �  �                  3   ����      $   K  �  ���                                                   � ߱            O   P  ��  ��                 l          \  d    L                                             ��                            ����                                            �           �   l       ��                  [  i  �               �                    O   ����    e�          O   ����    R�          O   ����    ��      �                      �          x  /	  f         H                      3   ����(  H     
   8                      3   ����T            h                      3   ����\      O   h  ��  ��  h               �          �  �    �                                             ��                            ����                                            �           �   l       ��                  s  �  �               `�                    O   ����    e�          O   ����    R�          O   ����    ��      �   	                   �          x  /	  }         �                      3   ����|  H     
   8                      3   �����            h                      3   �����  �    ~  �  �      �      4   �����      O     ��  ��  �      O   �  ��  ��  �             	  (                                                         	     ��                            ����                                            �           �   l       ��                  �  �  �               L_�                    O   ����    e�          O   ����    R�          O   ����    ��      i   
                   �          x  /	  �         $	                      3   �����  H     
   8                      3   ����0	            h                      3   ����8	      O   �  ��  ��  D	             
  �          �  �    �                                        
     ��                            ����                                                      �   l       ��                 �  �  �               0I�                    O   ����    e�          O   ����    R�          O   ����    ��      �       �              �          �                      �          �    �     �      X	      4   ����X	                �                      ��                  �  �                  �J�                       �  0  d    �  �  D  \  t	      4   ����t	                T                      ��                  �  �                  ���                       �  �  �    �  p  �      �	      4   �����	      $  �  �  ���                       �	                         � ߱              �  �        
      4   ����
      $  �  0  ���                       P
                         � ߱              �  x  �      �
      4   �����
                                      ��                  �  �                  |K�                       �  �  \  $  �  0  ���                       �
     
                    � ߱        �    �  x  �      �
      4   �����
      $  �  �  ���                       0                         � ߱              �  �        �      4   �����      $  �  8  ���                       �                         � ߱        �    �  �  �      @      4   ����@      $  �  �  ���                       �                         � ߱        �    �          �      4   �����      $  �  @  ���                                                � ߱        8  @         $          d  @         P              � ߱            $   �  l  ���                           O   �  ��  ��  p               |          \  l   @ ,                                      
                     0              0     �     ��                            ����                                            �           �   l       ��                  �    �               �L�                    O   ����    e�          O   ����    R�          O   ����    ��          /	    �      �                          3   �����         
                         3   ����    ��                            ����                                            �           �   l       ��                      �               pV�                    O   ����    e�          O   ����    R�          O   ����    ��      %                      �              /           4                      3   ����  H     
   8                      3   ����@            h                      3   ����H               �          �  �    �                                             ��                            ����                                            T          �   l       ��                  (  :  �                W�                    O   ����    e�          O   ����    R�          O   ����    ��      9       �              �          E     
               �   
       i       8                      R     
                 ,  
       �  /	  6  �     �  t                      3   ����T  �     
   �                      3   �����  �        �                      3   �����        
                         3   �����  P        @                      3   �����         
   p  �                  3   �����      $   6  �  ���                               
                    � ߱            O   9  ��  ��  �               �          d  x   T $                          
                           
                 $   4   D          $   4   D    � �        ��                            ����                                            �           �   l       ��                  D  \  �               T�                    O   ����    e�          O   ����    R�          O   ����    ��      i                      �              /	  Y         �                      3   �����  H     
   8                      3   �����            h                      3   ����                �          �  �    �                                             ��                            ����                                            �           �   l       ��                  f  u  �               �'�                    O   ����    e�          O   ����    R�          O   ����    ��      %                      �              /	  r         ,                      3   ����  H     
   8                      3   ����8            h                      3   ����@               �          �  �    �                                             ��                            ����                                            �           �   l       ��                    �  �               �0�                    O   ����    e�          O   ����    R�          O   ����    ��      %                      �              /  �         l                      3   ����L  H     
   8                      3   ����x            h                      3   �����               �          �  �    �                                             ��                            ����                                                        �   l       ��                  �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                            �           �   l       ��                 �  _  �                t�                    O   ����    e�          O   ����    R�          O   ����    ��         /	    �      �   �                      3   �����                                  3   �����  d      <  L      �      4   �����      O     ��  ��    �  /     �     �                          3   ����            �                      3   ����4  (  $    �  ���                       P                         � ߱        L  /	  !  T     d  �                      3   �����  �     
   �                      3   �����  �        �                      3   �����            �  �                  3   �����      $   !     ���                                                   � ߱        �    #  h  x      �      4   �����      O   $  ��  ��    $     
                    � ߱        �  $  %  �  ���                       �    &          P      4   ����P      	  '  H                                    X  3   �����  h  3   �����  x  3   �����  �  3   �����  �  3   �����  �  3   �����      3   �����  $  /   -  �     �                          3   �����                                  3   ����  |  $  .  P  ���                       ,       
       
           � ߱             /  �  �      0      4   ����0      $  /  �  ���                       P       
       
           � ߱        4    0    �      d      4   ����d                �                      ��                  0  5                  ۢ                       0  ,  �  /	  1  �     �  �                      3   �����                                3   �����            4  D                  3   �����      $   1  p  ���                                                   � ߱              3  �  �      �      4   �����      /   4  �                               3   �����            $                      3   ����  �    R  P  `      0      4   ����0      /  S  �     �  X                      3   ����@            �                      3   ����`      /  [  �     	  �                      3   ����|            (	                      3   �����               d
          
  8
  , � l	                                                                                               
                                                      ,   <   L   \   l   |   �   �   �   �       ,   <   L   \   l   |   �   �   �   �      �  � ��      ��                            ����                                            �           �   l       ��                  e  q  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��      �      o  �� �                       p  �         �      4   �����      �   p  �    ��                              ��        3                   ����                                            �           �   l       ��                  w  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��          �               � ߱            h   �  �    �                          ��                              ��        3                   ����                                                        �   l       ��                  �  �  �               ���                    O   ����    e�          O   ����    R�          O   ����    ��        ��                            ����                                                        �   l       ��                  �  �  �               4��                    O   ����    e�          O   ����    R�          O   ����    ��      !     
  �              �   
       �                      �                       p          X  d   , 8            
                                            �       ��                            ����                               D   d d     �   ��"G#  � �                                               3                     $                                                   d     D                                                                 `  d d ` `                                                                 $         ` `      \  d d C                                                  <      �         �       D                                                                    TXS lh_handle BUTTON-4 Regresar aplic/pda/back.bmp F-Main D:\newsie\on_in_co\aplic\pda\f-cancel-sku.w should only be RUN PERSISTENT. adm-object-hdl adm-query-opened adm-row-avail-state adm-initial-lock adm-new-record adm-updating-record adm-check-modified-all adm-broker-hdl TYPE ADM-Broker ADM1.1` SmartFrame` FRAME` NO ` `  Layout,Hide-on-Init` ``````````` ^^ ^ ADM-APPLY-ENTRY ADM-DESTROY disable-fields ENABLED=no ADM-DISABLE ADM-EDIT-ATTRIBUTE-LIST ENABLED=yes ADM-ENABLE exit ADM-EXIT ADM-HIDE ADM-INITIALIZE cntr ADM-SHOW-ERRORS ADM-UIB-MODE ADM-VIEW p-method-name ADM-ERROR DISPATCH p-attr-name GET-ATTRIBUTE p-attr-list GET-ATTRIBUTE-LIST p-state NEW-STATE p-method NOTIFY SET-ATTRIBUTE-LIST p-row p-col parent-hdl WINDOW DIALOG-BOX SET-POSITION CURRENT-PAGE=0,ADM-OBJECT-HANDLE= ADM-CHANGE-PAGE p-page# DELETE-PAGE p-proc-name p-parent-hdl p-proc-hdl INIT-OBJECT p-page-list INIT-PAGES SELECT-PAGE VIEW-PAGE Cancel-SKU ADM-CREATE-OBJECTS tbl-list rowid-list row-avail-cntr row-avail-rowid row-avail-enabled link-handle record-source-hdl different-row key-name key-value check FIELDS-ENABLED YES RECORD-SOURCE row-available in  encountered more than one RECORD-SOURCE. The first -   - will be used. Key-Name Key-Value="&1" display-fields row-available ADM-ROW-AVAILABLE DISABLE_UI ENABLE_UI SEND-RECORDS p-issuer-hdl STATE-CHANGED Button 4 �  h      0       �    X                     H                   adm-apply-entry �  �     �                     �                   adm-destroy +  -  `   �                     �                   adm-disable ?  @  C  E  F  �   <                    $                  adm-edit-attribute-list W  \  ]  �   �                    x                  adm-enable  q  t  x  z  {  H  �                    �                  adm-exit    �  �  �  �                                        adm-hide    �  �  �  d                    T                  adm-initialize  �  �            �     cntr    $  �     	   l          �                  adm-show-errors �  �  �  �  �  �       
                                 adm-UIB-mode    �  �  �  `                    T                  adm-view    �  �            �        p-method-name   $  �            h      �                  dispatch                    �        p-attr-name �  <            �      ,                  get-attribute   ,  /  0            `        p-attr-list �  �            H      �                  get-attribute-list  K  P  Q            �        p-state l              �                        new-state   f  h  i      	      <        p-method    �  �            $      x                  notify  }  ~    �  �      
      �        p-attr-list H  �            �      �                  set-attribute-list  �  �  �              
   parent-hdl  H        @        p-row             `        p-col   �  �          (      �                  set-position    �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  �  h  4                    $                  adm-change-page               T        p-page# �  �            <      �                  delete-page     �        �        p-proc-name �        �       
 p-parent-hdl                    p-attr-list           (       
 p-proc-hdl  \  p            �      d                  init-object 6  9  :            �        p-page-list 4  �            |      �                  init-pages  Y  \            �        p-page# �  @	            �      4	                  select-page r  u            `	        p-page# 	  �	            H	      �	                  view-page   �  �  h	  �	                                        �  �  �	  (
                    
                  adm-create-objects  �  L
        @
     tbl-list    l
        `
     rowid-list  �
        �
     row-avail-cntr  �
        �
     row-avail-rowid �
        �
     row-avail-enabled   �
        �
     link-handle $          
   record-source-hdl   H     	   8     different-row   h     
   \     key-name              |     key-value   �	  �        ,
          �                  adm-row-available           !  #  $  %  &  '  -  .  /  0  1  3  4  5  R  S  [  _  �  `                    T                  disable_UI  o  p  q  $  �                    �                  enable_UI   �  �  l  �                    �                  send-records    �                 
 p-issuer-hdl              4        p-state �  |            �      l                  state-changed   �  <          �                              �          �  
   lh_handle   �       �  
   adm-object-hdl              adm-query-opened    D       0     adm-row-avail-state l       X     adm-initial-lock    �       �     adm-new-record  �       �     adm-updating-record �       �     adm-check-modified-all            �  
   adm-broker-hdl  2   w   x   z   {   �  �  �  �  �  �  �  �  �  �  �      �<  $d:\newsie\on_in_co\src\adm\template\row-end.i    H  q?  $d:\newsie\on_in_co\src\adm\template\row-head.i   �  �  "d:\newsie\on_in_co\src\adm\method\containr.i �  b�  "d:\newsie\on_in_co\src\adm\method\smart.i    �  ��  "d:\newsie\on_in_co\src\adm\method\attribut.i 0  I�    D:\newsie\on_in_co\aplic\pda\f-cancel-sku.w      �   ^      �     ;     �  �   7      �     �     �  �   �      �  Z   �     �  9   r                     �            �      