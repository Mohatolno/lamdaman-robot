Caml1999N025����            -src/world.mli����  �(    \  Y�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���-ppxlib_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����o�@�@@@@�@@@�@�������*ocaml.text���@@ ���@@ �A�������	�

   Ce module fournit les types et les opérations pour représenter et
   faire évoluer le monde. Les fonctions documentées *peuvent* vous
   servir. Celles qui ne le sont pas ne devraient pas vous servir.

@��-src/world.mliA@@�G � �@@@@��A@@�G � �@@���@@ ���@@ �A��
A@@�G � �@���A�    �$kind��J � ��J � @@@��Р&Ground��K�K@������%float��&K�'K@@��)K�*K@@@@@��,K�-K@���)ocaml.doc���@@ ���@@ �A�������	� des espaces avec plus ou moins de frottements : [Ground f] implique
      que la vitesse est multipliée par [f] lorsque l'on passe par ici.
      [f] est compris entre 0.5 et 1.5 ... @��>L�?N��@@@@��AL�BN��@@���@@ ���@@ �A@�Р$Hell��KP���LP��@�@@��OP���PP��@���#���@@ ���@@ �A�������	W et aussi des endroits où le robot ne doit pas s'aventurer sans
      être détruit. @��`Q���aR(@@@@@��cQ���dR(@@@��@@ ��@@ �A@@A@���:��@@ ��@@ �A�������7 Dans monde, il y a ...@��wI � ��xI � �@@@@��zI � ��{I � �@@��@@ ��@@ �A���(deriving���SAD��SAL@��������&yojson���SAM��SAS@���SAM��SAS@@@@���SAM��SAS@@���SAA��SAT@@���J � ���SAT@@���J � ���SAT@���Р.kind_to_yojson��C@@ ��D@@ �A��@��������J � ���J � @@���J � ���SAT@@@������&Yojson$Safe!t���J � ���SAT@@���J � ���SAT@@@���J � ���SAT@@@@@���J � ���SAT@���J � ���SAT@���Р.kind_of_yojson��p@@ ��q@@ �A��@������&Yojson$Safe!t���J � ���SAT@@���J � ���SAT@@@�����;Ppx_deriving_yojson_runtime(error_or���J � ���SAT@��������J � ���J � @@���J � ���SAT@@@@���J � ���SAT@@@��J � ��SAT@@@@@��J � ��SAT@��J � ��SAT@���A�    �!t��Vv{�Vv|@@@��Р%space��W���W��@@�����%Space!t��#W���$W��@�����$kind��,W���-W��@@��/W���0W��@@@@��2W���3W��@@@��5W���6W��@���	���@@ ���@@ �A�������? des polygones de type [kind], @��FW���GW��@@@@��IW���JW��@@���@@ ���@@ �A@�Р%trees��SX���TX��@@����$list��[X���\X��@�����$tree��dX���eX��@@��gX���hX��@@@@��jX���kX��@@@��mX���nX��@���A��@@ ��@@ �A�������	  des arbres de Böhms,          @��~X���X�@@@@���X����X�@@��@@ �� @@ �A@�Р*microcodes���Y��Y(@@����$list���Y;��Y?@�����.left_microcode���Y,��Y:@@���Y,��Y:@@@@���Y,��Y?@@@���Y��Y@@���y��F@@ ��G@@ �A�������	! des microcodes laissés là,    @���YA��Yg@@@@���YA��Yg@@��W@@ ��X@@ �A@�Р%teams���Zhl��Zhq@@����$list���Zh��Zh�@�����$team���Zhz��Zh~@@���Zhz��Zh~@@@@���Zhz��Zh�@@@���Zhl��Zh�@������~@@ ��@@ �A�������	  des équipes de robots,        @���Zh���Zh�@@@@���Zh���Zh�@@���@@ ���@@ �A@�Р*visibility���[����[��@@����%float��[���[��@@��[���[��@@@��	[���
[��@���ݰ��@@ ���@@ �A�������	  une distance où on y voit,    @��[���[�@@@@��[���[�@@���@@ ���@@ �A@�Р%epoch��'\�(\@@����#int��/\�0\@@��2\�3\@@@��5\�6\@���	���@@ ���@@ �A�������	  un âge,                       @��F\)�G\N@@@@��I\)�J\N@@���@@ ���@@ �A@�Р+end_of_time��S]OS�T]O^@@����#int��[]Oa�\]Od@@��^]Oa�_]Od@@@��a]OS�b]Oe@���5��@@ ��@@ �A�������	  une durée de vie.             @��r]Ov�s]O�@@@@��u]Ov�v]O�@@��@@ ��@@ �A@@A@���L��@@ ��@@ �A�������: Le monde est défini par:@���UVV��UVu@@@@���UVV��UVu@@��*@@ ��+@@ �A@���Vvv��^��@�    �$tree���a����a��@@@��Р-tree_position���b����b��@@�����%Space(position���b����b��@@���b����b��@@@���b����b��@������R@@ ��S@@ �A�������	  une position,                  @���b����b�@@@@���b����b�@@��c@@ ��d@@ �A@�Р(branches���c��c @@����#int���c(��c+@@���c(��c+@@@���c��c,@������~@@ ��@@ �A�������	  et un nombre de branches >= 0. @���c;��c`@@@@���c;��c`@@���@@ ���@@ �A@@A@���Ȱ��@@ ���@@ �A�������5 Un arbre de Böhm a @��`���`��@@@@��`���	`��@@���@@ ���@@ �A@��a���dab@�    �.left_microcode��g���g��@@@��Р)microcode��h���h��@@����)microcode��%h���&h��@@��(h���)h��@@@��+h���,h��@�������@@ ���@@ �A�������2 un message,      @��<h���=h�@@@@��?h���@h�@@���@@ ���@@ �A@�Р2microcode_position��Ii	�Ji	@@�����%Space(position��Si	"�Ti	0@@��Vi	"�Wi	0@@@��Yi	�Zi	1@���-���@@ ���@@ �A�������2 sa position,     @��ji	2�ki	I@@@@��mi	2�ni	I@@��@@ ��@@ �A@�Р(duration��wjJN�xjJV@@�����%Space(duration���jJc��jJq@@���jJc��jJq@@@���jJN��jJr@���[��(@@ ��)@@ �A�������3 sa durée de vie. @���jJs��jJ�@@@@���jJs��jJ�@@��9@@ ��:@@ �A@@A@���r��?@@ ��@@@ �A�������	G Les robots peuvent laisser des microcodes sur le sol. Ils contiennent @���fdd��fd�@@@@���fdd��fd�@@��P@@ ��Q@@ �A@���g����k��@�    �)microcode���n����n��@@@��Р)MicroAtom���o����o��@������#int���o����o��@@���o����o��@@@@@���o����o��@������x@@ ��y@@ �A�������	& dont les atomes sont des entiers, et @���o����o�@@@@���o����o�@@���@@ ���@@ �A@�Р)MicroList���p��p$@������$list���p2� p6@�����)microcode��p(�	p1@@��p(�p1@@@@��p(�p6@@@@@��p�p6@������@@ ���@@ �A�������	' que l'on peut structurer à sa guise. @��"p:�#pf@@@@��%p:�&pf@@���@@ ���@@ �A@@A@�������@@ ���@@ �A�������	# Un [microcode] est une donnée... @��9m���:m��@@@@��<m���=m��@@���@@ ���@@ �A@��Bn���Cp6@�    �$team��Is}��Js}�@@@��Р/team_identifier��Qt���Rt��@@����/team_identifier��Yt���Zt��@@��\t���]t��@@@��_t���`t��@���3�� @@ ��@@ �A�������	# a un identifiant unique.          @��pt���qt��@@@@��st���tt��@@��@@ ��@@ �A@�Р&robots��}u���~u��@@����$list���u����u��@�����%robot���u����u��@@���u����u��@@@@���u����u��@@@���u����u��@���k��8@@ ��9@@ �A�������	$ est formée d'une liste de robots. @���u���u�*@@@@���u���u�*@@��I@@ ��J@@ �A@�Р)spaceship���v+/��v+8@@�����%Space(position���v+A��v+O@@���v+A��v+O@@@���v+/��v+P@������f@@ ��g@@ �A�������	$ et possède un vaisseau spatial.   @���v+R��v+{@@@@���v+R��v+{@@��w@@ ��x@@ �A@@A@������}@@ ��~@@ �A�������/ Une équipe...@���rhh��rh|@@@@���rhh��rh|@@���@@ ���@@ �A@���s}}��w|}@�    �%robot���z����z��@@@��Р0robot_identifier��{���{��@@����0robot_identifier��{���{��@@��{���{��@@@��{���{��@������@@ ���@@ �A�������; un identifiant unique     @��${���%{��@@@@��'{���({��@@���@@ ���@@ �A@�Р5robot_team_identifier��1|���2|�	@@����/team_identifier��9|�	�:|�	"@@��<|�	�=|�	"@@@��?|���@|�	#@������@@ ���@@ �A�������< dans son équipe,          @��P|�	%�Q|�	F@@@@��S|�	%�T|�	F@@���@@ ���@@ �A@�Р.robot_position��]}	G	K�^}	G	Y@@�����%Space(position��g}	G	c�h}	G	q@@��j}	G	c�k}	G	q@@@��m}	G	K�n}	G	r@���A��@@ ��@@ �A�������; une position,             @��~}	G	u�}	G	�@@@@���}	G	u��}	G	�@@��@@ �� @@ �A@�Р/robot_spaceship���~	�	���~	�	�@@�����%Space(position���~	�	���~	�	�@@���~	�	���~	�	�@@@���~	�	���~	�	�@���o��<@@ ��=@@ �A�������; un vaisseau,              @���~	�	���~	�	�@@@@���~	�	���~	�	�@@��M@@ ��N@@ �A@�Р+robot_angle���	�	���	�	�@@�����%Space%angle���	�
��	�
@@���	�
��	�
@@@���	�	���	�
@������j@@ ��k@@ �A�������; une direction,            @���	�
��	�
3@@@@���	�
��	�
3@@��{@@ ��|@@ �A@�Р+robot_speed��� @
4
8�� @
4
C@@�����%Space%speed��� @
4
P�� @
4
[@@��� @
4
P�� @
4
[@@@��� @
4
8�� @
4
\@���˰��@@ ���@@ �A�������; une vitesse,              @�� @
4
b�	 @
4
�@@@@�� @
4
b� @
4
�@@���@@ ���@@ �A@�Р,robot_energy�� A
�
�� A
�
�@@����#int�� A
�
�� A
�
�@@��  A
�
��! A
�
�@@@��# A
�
��$ A
�
�@�������@@ ���@@ �A�������< une énergie positive,     @��4 A
�
��5 A
�
�@@@@��7 A
�
��8 A
�
�@@���@@ ���@@ �A@�Р+robot_score��A B
�
��B B
�
�@@����#int��I B
�
��J B
�
�@@��L B
�
��M B
�
�@@@��O B
�
��P B
�
�@���#���@@ ���@@ �A�������; un score,                 @��` B
��a B
�!@@@@��c B
��d B
�!@@��@@ ��@@ �A@�Р+robot_bonus��m C"&�n C"1@@����#int��u C">�v C"A@@��x C">�y C"A@@@��{ C"&�| C"B@���O��@@ ��@@ �A�������; un bonus potentiel,       @��� C"P�� C"p@@@@��� C"P�� C"p@@��-@@ ��.@@ �A@�Р*robot_down��� Dqu�� Dq@@����$bool��� Dq��� Dq�@@��� Dq��� Dq�@@@��� Dqu�� Dq�@���{��H@@ ��I@@ �A�������> mais peut être désactivé. @��� Dq��� Dq�@@@@��� Dq��� Dq�@@��Y@@ ��Z@@ �A@@A@������_@@ ��`@@ �A�������5 Un λman possède : @���y��y�@@@@���y��y�@@��p@@ ��q@@ �A@���z���� E��@�    �0robot_identifier��� H���� H�@@@@A�����#int��� H��� H�@@��� H��� H�@@@�������@@ ���@@ �A�������	+ Les identifiants sont de simples entiers. @��� G���� G��@@@@��� G���  G��@@���@@ ���@@ �A@�� H��� H�@�    �/team_identifier�� H�� H�%@@@@A�����#int�� H�(� H�+@@�� H�(� H�+@@@���(deriving�� I,/�  I,7@��������&yojson��+ I,8�, I,>@��. I,8�/ I,>@@@@��1 I,8�2 I,>@@��4 I,,�5 I,?@@��7 H��8 I,?@@��:Vvv�; I,?@���Р)to_yojson���@@ ���@@ �A��@����<��LVv{�MVv|@@��OVvv�P I,?@@@�����������XVvv�Y I,?@@��[Vvv�\ I,?@@@��^Vvv�_ I,?@@@@@��aVvv�b I,?@��dVvv�e I,?@���Р)of_yojson��@@ ��	@@ �A��@�����������xVvv�y I,?@@��{Vvv�| I,?@@@����������Vvv�� I,?@�����{���Vv{��Vv|@@���Vvv�� I,?@@@@���Vvv�� I,?@@@���Vvv�� I,?@@@@@���Vvv�� I,?@���Vvv�� I,?@���Р.tree_to_yojson��>@@ ��?@@ �A��@�������a����a��@@���Vvv�� I,?@@@������������Vvv�� I,?@@���Vvv�� I,?@@@���Vvv�� I,?@@@@@���Vvv�� I,?@���Vvv�� I,?@���Р.tree_of_yojson��h@@ ��i@@ �A��@������������Vvv�� I,?@@���Vvv�� I,?@@@����������Vvv�� I,?@�����S���a����a��@@���Vvv�� I,?@@@@���Vvv�� I,?@@@���Vvv�� I,?@@@@@���Vvv�� I,?@���Vvv�� I,?@���Р8left_microcode_to_yojson���@@ ���@@ �A��@�������g���g��@@��Vvv� I,?@@@������[ZY��Vvv� I,?@@��Vvv� I,?@@@��Vvv� I,?@@@@@��!Vvv�" I,?@��$Vvv�% I,?@���Р8left_microcode_of_yojson���@@ ���@@ �A��@������XWV��8Vvv�9 I,?@@��;Vvv�< I,?@@@�����UT��CVvv�D I,?@�����7��Kg���Lg��@@��NVvv�O I,?@@@@��QVvv�R I,?@@@��TVvv�U I,?@@@@@��WVvv�X I,?@��ZVvv�[ I,?@���Р3microcode_to_yojson���@@ ���@@ �A��@�������ln���mn��@@��oVvv�p I,?@@@�����������xVvv�y I,?@@��{Vvv�| I,?@@@��~Vvv� I,?@@@@@���Vvv�� I,?@���Vvv�� I,?@���Р3microcode_of_yojson��	(@@ ��	)@@ �A��@������������Vvv�� I,?@@���Vvv�� I,?@@@����������Vvv�� I,?@���������n����n��@@���Vvv�� I,?@@@@���Vvv�� I,?@@@���Vvv�� I,?@@@@@���Vvv�� I,?@���Vvv�� I,?@���Р.team_to_yojson��	^@@ ��	_@@ �A��@��������s}���s}�@@���Vvv�� I,?@@@���������Vvv�� I,?@@���Vvv�� I,?@@@���Vvv�� I,?@@@@@���Vvv�� I,?@���Vvv�� I,?@���Р.team_of_yojson��	�@@ ��	�@@ �A��@���������Vvv�� I,?@@���Vvv�� I,?@@@�������	Vvv�	 I,?@�����ð�	s}��	s}�@@��	Vvv�	 I,?@@@@��	Vvv�	 I,?@@@��	Vvv�	 I,?@@@@@��	Vvv�	 I,?@��	Vvv�	 I,?@���Р/robot_to_yojson��	�@@ ��	�@@ �A��@����0��	,z���	-z��@@��	/Vvv�	0 I,?@@@������{zy��	8Vvv�	9 I,?@@��	;Vvv�	< I,?@@@��	>Vvv�	? I,?@@@@@��	AVvv�	B I,?@��	DVvv�	E I,?@���Р/robot_of_yojson��	�@@ ��	�@@ �A��@������xwv��	XVvv�	Y I,?@@��	[Vvv�	\ I,?@@@�����ut��	cVvv�	d I,?@�����o��	kz���	lz��@@��	nVvv�	o I,?@@@@��	qVvv�	r I,?@@@��	tVvv�	u I,?@@@@@��	wVvv�	x I,?@��	zVvv�	{ I,?@���Р:robot_identifier_to_yojson��
@@ ��
@@ �A��@�������	� H���	� H�@@��	�Vvv�	� I,?@@@��������ٰ�	�Vvv�	� I,?@@��	�Vvv�	� I,?@@@��	�Vvv�	� I,?@@@@@��	�Vvv�	� I,?@��	�Vvv�	� I,?@���Р:robot_identifier_of_yojson��
H@@ ��
I@@ �A��@��������ְ�	�Vvv�	� I,?@@��	�Vvv�	� I,?@@@������԰�	�Vvv�	� I,?@��������	� H���	� H�@@��	�Vvv�	� I,?@@@@��	�Vvv�	� I,?@@@��	�Vvv�	� I,?@@@@@��	�Vvv�	� I,?@��	�Vvv�	� I,?@���Р9team_identifier_to_yojson��
~@@ ��
@@ �A��@������	� H��	� H�%@@��	�Vvv�	� I,?@@@������	;	:	9��	�Vvv�	� I,?@@��	�Vvv�	� I,?@@@��	�Vvv�	� I,?@@@@@��
Vvv�
 I,?@��
Vvv�
 I,?@���Р9team_identifier_of_yojson��
�@@ ��
�@@ �A��@������	8	7	6��
Vvv�
 I,?@@��
Vvv�
 I,?@@@�����	5	4��
#Vvv�
$ I,?@����� ��
+ H��
, H�%@@��
.Vvv�
/ I,?@@@@��
1Vvv�
2 I,?@@@��
4Vvv�
5 I,?@@@@@��
7Vvv�
8 I,?@��
:Vvv�
; I,?@���Р$load��
C M���
D M��@��@����&string��
M M���
N M��@@��
P M���
Q M��@@@����!t��
X M���
Y M��@@��
[ M���
\ M��@@@��
^ M���
_ M��@@@@���
2��
�@@ �� @@ �A�������	Q [load filename] charge le fichier JSON [filename] qui représente un
    monde. @��
o KAA�
p L��@@@@��
r KAA�
s L��@@��@@ ��@@ �A@��
x M���
y M��@��
{ M���
| M��@���Р$save��
� P�
� P	@��@����&string��
� P�
� P@@��
� P�
� P@@@��@����!t��
� P�
� P@@��
� P�
� P@@@����$unit��
� P�
� P@@��
� P�
� P@@@��
� P�
� P@@@��
� P�
� P@@@@���
���P@@ ��Q@@ �A�������	K [save filename world] sauvegarde [world] dans le fichier JSON [filename]. @��
� O���
� O� @@@@��
� O���
� O� @@��a@@ ��b@@ �A@��
� P�
� P@��
� P�
� P@���Р'tree_at��
� Snr�
� Sny@��@����$list��
� Sn��
� Sn�@�����$tree��
� Sn|�
� Sn�@@��
� Sn|�
� Sn�@@@@��
� Sn|�
� Sn�@@@��@�����%Space(position��
� Sn��
� Sn�@@��
� Sn��
� Sn�@@@����&option�� Sn�� Sn�@�����$tree�� Sn�� Sn�@@�� Sn�� Sn�@@@@�� Sn�� Sn�@@@�� Sn�� Sn�@@@�� Sn|� Sn�@@@@���
���@@ ���@@ �A�������	G [tree_at world pos] renvoie un arbre proche de [pos], s'il y en a un. @��+ R!!�, R!m@@@@��. R!!�/ R!m@@���@@ ���@@ �A@��4 Snn�5 Sn�@��7 Snn�8 Sn�@���Р.tree_positions��@ V���A V�@��@����$list��J V��K V�@�����$tree��S V��T V�@@��V V��W V�@@@@��Y V��Z V�@@@����$list��a V�*�b V�.@������%Space(position��l V��m V�)@@��o V��p V�)@@@@��r V��s V�.@@@��u V��v V�.@@@@���I��@@ ��@@ �A�������	K [tree_positions trees] renvoie toutes les positions d'une liste d'arbres. @��� U���� U��@@@@��� U���� U��@@��'@@ ��(@@ �A@��� V���� V�.@��� V���� V�.@���Р+update_tree��� Z���� Z��@��@����!t��� Z���� Z��@@��� Z���� Z��@@@��@����$tree��� Z���� Z��@@��� Z���� Z��@@@��@����$tree��� Z���� Z��@@��� Z���� Z��@@@����!t��� Z���� Z��@@��� Z���� Z��@@@��� Z���� Z��@@@��� Z���� Z��@@@��� Z���� Z��@@@@������w@@ ��x@@ �A�������	R [update_tree world tree tree'] renvoie [world] où [tree'] remplace
    [tree']. @��� X00�� Yx�@@@@��� X00�� Yx�@@���@@ ���@@ �A@��� Z���� Z��@��� Z���� Z��@���Р2number_of_branches��� ^�� ^+@��@����!t�� ^.� ^/@@��	 ^.�
 ^/@@@����#int�� ^3� ^6@@�� ^3� ^6@@@�� ^.� ^6@@@@������@@ ���@@ �A�������	] [number_of_branches world] compte le nombre de branches d'arbres de Böhm
    dans [world]. @��( \���) ] @@@@��+ \���, ] @@���@@ ���@@ �A@��1 ^�2 ^6@��4 ^�5 ^6@���Р1size_of_microcode��= b���> b��@��@����)microcode��G b���H b��@@��J b���K b��@@@����#int��R b���S b��@@��U b���V b��@@@��X b���Y b��@@@@���,���@@ ���@@ �A�������	� [size_of_microcode m] est la taille d'un message. Plus un message est
    gros et plus il coûte cher à produire en termes d'énergie de robot. @��i `88�j a��@@@@��l `88�m a��@@��
@@ ��@@ �A@��r b���s b��@��u b���v b��@���Р#put��~ g��� g��@��@����!t��� g���� g��@@��� g���� g��@@@��@����)microcode��� g���� g��@@��� g���� g��@@@��@�����%Space(duration��� g���� g��@@��� g���� g��@@@��@�����%Space(position��� g���� g��@@��� g���� g��@@@����!t��� g���� g��@@��� g���� g��@@@��� g���� g��@@@��� g���� g��@@@��� g���� g��@@@��� g���� g��@@@@������n@@ ��o@@ �A�������	� [put world m d p] laisse un message [m] à une position [p]. Il
    s'autodétruira dans [d] unités de temps, sauf si on place un autre
    message identique à cette même position. @��� d���� f��@@@@��� d���� f��@@��@@ ���@@ �A@��� g���� g��@��� g���� g��@���A�    �+observation��� i��� i�@@@��Р%trees��� k�� k@@����$list�� k'� k+@�����$tree�� k"� k&@@�� k"� k&@@@@�� k"� k+@@@�� k� k,@������@@ ���@@ �A�������	% Les arbres de Böhm.                @��' k7�( ka@@@@��* k7�+ ka@@���@@ ���@@ �A@�Р(messages��4 lbf�5 lbn@@����$list��< lb��= lb�@�����.left_microcode��E lbs�F lb�@@��H lbs�I lb�@@@@��K lbs�L lb�@@@��N lbf�O lb�@���"���@@ ���@@ �A�������	& Les microcodes à proximité.        @��_ lb��` lb�@@@@��b lb��c lb�@@�� @@ ��@@ �A@�Р&around��l m���m m��@@�����%Space!t��v m���w m��@�����$kind�� m���� m��@@��� m���� m��@@@@��� m���� m��@@@��� m���� m��@���\��)@@ ��*@@ �A�������	$ L'espace visible par le robot.     @��� m���� m�@@@@��� m���� m�@@��:@@ ��;@@ �A@�Р%speed��� n�� n@@�����%Space%speed��� n�� n @@��� n�� n @@@��� n�� n!@������W@@ ��X@@ �A�������	$ La vitesse courante du robot.      @��� n*�� nS@@@@��� n*�� nS@@��h@@ ��i@@ �A@�Р)max_speed��� oTX�� oTa@@�����%Space%speed��� oTe�� oTp@@��� oTe�� oTp@@@��� oTX�� oTq@�������@@ ���@@ �A�������	$ Vitesse maximale au point courant. @��� oTz�� oT�@@@@��� oTz�� oT�@@���@@ ���@@ �A@�Р%angle�� p��� p��@@�����%Space%angle�� p��� p��@@�� p��� p��@@@�� p��� p��@������@@ ���@@ �A�������	$ La direction courante du robot.    @��# p���$ p��@@@@��& p���' p��@@���@@ ���@@ �A@�Р(position��0 q���1 q� @@�����%Space(position��: q��; q�@@��= q��> q�@@@��@ q���A q�@������@@ ���@@ �A�������	$ La position courante du robot.     @��Q q��R q�C@@@@��T q��U q�C@@���@@ ���@@ �A@�Р)spaceship��^ rDH�_ rDQ@@�����%Space(position��h rDU�i rDc@@��k rDU�l rDc@@@��n rDH�o rDd@���B��@@ ��@@ �A�������	$ Position du vaisseau.              @�� rDj�� rD�@@@@��� rDj�� rD�@@�� @@ ��!@@ �A@�Р&energy��� s���� s��@@����#int��� s���� s��@@��� s���� s��@@@��� s���� s��@���n��;@@ ��<@@ �A�������	% L'énergie courante du robot.       @��� s���� s��@@@@��� s���� s��@@��L@@ ��M@@ �A@�Р%score��� t���� t��@@����#int��� t���� t��@@��� t���� t��@@@��� t���� t��@������g@@ ��h@@ �A�������	$ Le score du robot.                 @��� t��� t�4@@@@��� t��� t�4@@��x@@ ��y@@ �A@�Р%epoch��� u59�� u5>@@����#int��� u5F�� u5I@@��� u5F�� u5I@@@��� u59�� u5J@@�Р*visibility��� vKO�� vKY@@����%float�� vK\� vKa@@�� vK\� vKa@@@�� vKO� vKb@@�Р)game_over�� wcg� wcp@@����$bool�� wct� wcx@@�� wct� wcx@@@�� wcg� wcy@@@A@���(deriving��# y|�$ y|�@��������&yojson��/ y|��0 y|�@��2 y|��3 y|�@@@@��5 y|��6 y|�@@��8 y||�9 y|�@@��; i���< y|�@@��> i���? y|�@���Р5observation_to_yojson���@@ ���@@ �A��@����]��P i��Q i�@@��S i���T y|�@@@�����������\ i���] y|�@@��_ i���` y|�@@@��b i���c y|�@@@@@��e i���f y|�@��h i���i y|�@���Р5observation_of_yojson��@@ ��@@ �A��@�����������| i���} y|�@@�� i���� y|�@@@���������� i���� y|�@��������� i��� i�@@��� i���� y|�@@@@��� i���� y|�@@@��� i���� y|�@@@@@��� i���� y|�@��� i���� y|�@���Р4world_of_observation��� |���� |��@��@����+observation��� |���� |��@@��� |���� |��@@@����!t��� |���� |��@@��� |���� |��@@@��� |���� |��@@@@������c@@ ��d@@ �A�������	9 Une observation fournit une vue partielle sur un monde. @��� {���� {��@@@@��� {���� {��@@��t@@ ��u@@ �A@��� |���� |��@��� |���� |��@���Р*make_robot��� AE�� AO@��@����/team_identifier��� �RT�� �Rc@@��� �RT�� �Rc@@@��@����/team_identifier��� �gi�  �gx@@�� �gi� �gx@@@��@�����%Space(position�� �|~� �|�@@�� �|~� �|�@@@��@�����%Space%angle�� �|�� �|�@@��  �|��! �|�@@@����%robot��( �|��) �|�@@��+ �|��, �|�@@@��. �|��/ �|�@@@��1 �|~�2 �|�@@@��4 �gi�5 �|�@@@��7 �RT�8 �|�@@@@������@@ ���@@ �A�������	> Cette fonction produit un robot, qu'il reste à initialiser. @��H ~���I ~�@@@@@��K ~���L ~�@@@���@@ ���@@ �A@��Q AA�R �|�@��T AA�U �|�@���Р$move��] �	�^ �	@��@����+observation��g �	�h �	@@��j �	�k �	@@@��@����%robot��t �	#�u �	(@@��w �	#�x �	(@@@��@�����%Space%angle��� �	,�� �	7@@��� �	,�� �	7@@@��@�����%Space%speed��� �	;�� �	F@@��� �	;�� �	F@@@����%robot��� �	J�� �	O@@��� �	J�� �	O@@@��� �	;�� �	O@@@��� �	,�� �	O@@@��� �	#�� �	O@@@��� �	�� �	O@@@@������M@@ ��N@@ �A�������	] Cette fonction déplace le robot en respectant la vitesse maximale
   autorisée observée. @��� ����� ��@@@@��� ����� ��@@��^@@ ��_@@ �A@��� �		�� �	O@��� �		�� �	O@���Р$bury��� �mq�� �mu@��@����%robot��� �mx�� �m}@@��� �mx�� �m}@@@����%robot��� �m��� �m�@@��� �m��� �m�@@@��� �mx�� �m�@@@@�������@@ ���@@ �A�������6 Désactive un robot. @��� �QQ�� �Ql@@@@�� �QQ� �Ql@@���@@ ���@@ �A@�� �mm� �m�@��
 �mm� �m�@���Р-robot_is_dead�� ���� ���@��@����%robot�� ���� ���@@��  ����! ���@@@����$bool��( ����) ���@@��+ ����, ���@@@��. ����/ ���@@@@������@@ ���@@ �A�������	$ Teste si un robot est désactivé. @��? ����@ ���@@@@��B ����C ���@@���@@ ���@@ �A@��H ����I ���@��K ����L ���@���Р+update_team��T �,0�U �,;@��@����!t��^ �,>�_ �,?@@��a �,>�b �,?@@@��@����$team��k �,C�l �,G@@��n �,C�o �,G@@@��@����$team��x �,K�y �,O@@��{ �,K�| �,O@@@����!t��� �,S�� �,T@@��� �,S�� �,T@@@��� �,K�� �,T@@@��� �,C�� �,T@@@��� �,>�� �,T@@@@���c��0@@ ��1@@ �A�������	Q [update_team world team team'] produit [world] où [team'] remplace
    [team]. @��� ����� �+@@@@��� ����� �+@@��A@@ ��B@@ �A@��� �,,�� �,T@��� �,,�� �,T@���Р,update_robot��� ����� ���@��@����!t��� ����� ���@@��� ����� ���@@@��@����%robot��� ����� ���@@��� ����� ���@@@��@����%robot��� ����� ���@@��� ����� ���@@@����!t��� ����� ���@@��� ����� ���@@@��� ����� ���@@@��� ����� ���@@@��� ����� ���@@@@���İ��@@ ���@@ �A�������	V [update_robot world robot robot'] produit [world] où [robot'] remplace
    [robot]. @�� �VV� ���@@@@�� �VV� ���@@���@@ ���@@ �A@��
 ���� ���@�� ���� ���@���Р+inside_hell�� � � �+@��@����!t��  �.�! �/@@��# �.�$ �/@@@��@�����%Space(position��/ �3�0 �A@@��2 �3�3 �A@@@����$bool��: �E�; �I@@��= �E�> �I@@@��@ �3�A �I@@@��C �.�D �I@@@@������@@ ���@@ �A�������	7 Teste si une position est dans une bouche de l'enfer. @��T ����U ��@@@@��W ����X ��@@���@@ ���@@ �A@��] ��^ �I@��` ��a �I@���Р-hell_segments��i ����j ���@��@����!t��s ����t ���@@��v ����w ���@@@����$list��~ ���� ���@������%Space'segment��� ����� ���@@��� ����� ���@@@@��� ����� ���@@@��� ����� ���@@@@���f��3@@ ��4@@ �A�������	3 Renvoie tous les segments des bouches de l'enfer. @��� �KK�� �K�@@@@��� �KK�� �K�@@��D@@ ��E@@ �A@��� ����� ���@��� ����� ���@���Р)suffering��� ����� ���@��@����!t��� ���� ��@@��� ���� ��@@@��@�����%Space(position��� ���� ��@@��� ���� ��@@@����%float��� ���� ��@@��� ���� ��@@@��� ���� ��@@@��� ���� ��@@@@�������@@ ���@@ �A�������	; Renvoie le niveau de souffrance à une certaine position. @��� ����� ���@@@@��� ����� ���@@���@@ ���@@ �A@��� ����  ��@�� ���� ��@��������@@ ���@@ �A�������	A Ces fonctions sont non documentées. Vous n'en avez pas besoin. @�� �%%� �%k@@@@�� �%%� �%k@@���@@ ���@@ �A�� �%%� �%k@���Р,forall_robot��' �mq�( �m}@��@����!t��1 ����2 ���@@��4 ����5 ���@@@��@��@����!t��@ ����A ���@@��C ����D ���@@@��@����%robot��M ����N ���@@��P ����Q ���@@@�����#Lwt!t��Z ����[ ���@��������%robot��f ����g ���@@��i ����j ���@@@�����$list��r ����s ���@���!a��y ����z ���@@@@��| ����} ���@@@@�� ����� ���@@@@��� ����� ���@@@��� ����� ���@@@��� ����� ���@@@�����#Lwt!t��� ����� ���@��������!t��� ����� ���@@��� ����� ���@@@�����$list��� ����� ���@���!a��� ����� ���@@@@��� ����� ���@@@@��� ����� ���@@@@��� ����� ���@@@��� ����� ���@@@��� ����� ���@@@@@��� �mm�� ���@��� �mm�� ���@���Р*map_robots��� ����� ���@��@��@����%robot��� ����� ���@@��� ����� ���@@@����%robot��� ����� ���@@��� ����� ���@@@��� ����� ���@@@��@����!t��� ����� ���@@��� ����� ���@@@����!t�� ���� ���@@�� ���� ���@@@�� ���� ���@@@��
 ���� ���@@@@@�� ���� ���@�� ���� ���@���Р.map_microcodes�� ���� ��@��@��@����.left_microcode��% ��
�& ��@@��( ��
�) ��@@@����.left_microcode��0 ���1 ��*@@��3 ���4 ��*@@@��6 ��
�7 ��*@@@��@����!t��@ ��/�A ��0@@��C ��/�D ��0@@@����!t��K ��4�L ��5@@��N ��4�O ��5@@@��Q ��/�R ��5@@@��T ��	�U ��5@@@@@��W ����X ��5@��Z ����[ ��5@���Р6remove_dead_microcodes��c �7;�d �7Q@��@����!t��m �7T�n �7U@@��p �7T�q �7U@@@����!t��x �7Y�y �7Z@@��{ �7Y�| �7Z@@@��~ �7T� �7Z@@@@@��� �77�� �7Z@��� �77�� �7Z@���Р,space_around��� �\`�� �\l@��@����!t��� �\o�� �\p@@��� �\o�� �\p@@@��@�������%float��� �\t�� �\y@@��� �\t�� �\y@@@�����%float��� �\|�� �\�@@��� �\|�� �\�@@@@��� �\t�� �\�@@@�����%Space!t��� �\��� �\�@�����$kind��� �\��� �\�@@��� �\��� �\�@@@@��� �\��� �\�@@@��� �\t�� �\�@@@��� �\o�� �\�@@@@@��� �\\�� �\�@��� �\\�� �\�@���Р-extends_world��� ����� ���@��@����!t��� ����� ���@@��� ����� ���@@@��@����!t��� ����� ���@@�� ���� ���@@@����!t��	 ����
 ���@@�� ���� ���@@@�� ���� ���@@@�� ���� ���@@@@@�� ���� ���@�� ���� ���@���Р1robot_perspective��! ����" ���@��@����!t��+ ����, ���@@��. ����/ ���@@@��@����$bool��8 ����9 ���@@��; ����< ���@@@��@����%robot��E ����F ���@@��H ����I ���@@@����+observation��P ����Q ���@@��S ����T ���@@@��V ����W ���@@@��Y ����Z ���@@@��\ ����] ���@@@@@��_ ����` ���@��b ����c ���@@