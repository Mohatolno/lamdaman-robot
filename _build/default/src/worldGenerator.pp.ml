Caml1999M025����            5src/worldGenerator.ml����  r�  �  R�  Q_�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���-ppxlib_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����o�@�@@@@�@@@�@�����@������"()��5src/worldGenerator.mlA@@�A@@@@��A@@�A@@@@@��������3Ppx_inline_test_lib'Runtime5set_lib_and_partition��A@@�A@@@��A@@�A@@@@@��@���)lambdaLib@��A@@�A@@@@@��@��� @��'A@@�(A@@@@@@��*A@@�+A@@@@@@��-A@@�.A@@@@��0A@@�1A@@@�����*ocaml.text���@@ ���@@ �A�������	�

   Ce module fournit un générateur aléatoire de mondes. Il est peu
   sophistiqué, c'est à vous de l'améliorer pour briller dans la tâche
   5 du projet.

@��DA@@�EG � �@@@@��GA@@�HG � �@@���@@ ���@@ �A��MA@@�NG � �@��������%World��YI � ��ZI � �@��\I � ��]I � �@@A��_I � ��`I � �@@��bI � ��cI � �@���@�����3default_end_of_time��nK � ��oK � �@��qK � ��rK � �@@@���&100000@��xL � ��yL � �@@@@��{K � ��|L � �@@��~K � ��L � �@���@�����2default_visibility���N � ���N � �@���N � ���N � �@@@���$100.@���O � ���O � �@@@@���N � ���O � �@@���N � ���O � �@���@�����'initial���Q � ��Q �@���Q � ��Q �@@@������%space���R��R@�����%Space%empty���R��R)@���R��R)@@@����%trees���S+/��S+4@����"[]���S+=��S+?@@���S+=��S+?@@@����%teams���TAE��TAJ@�������TAS��TAU@@���TAS��TAU@@@����%epoch���UW[��UW`@���!0@���UWi��UWj@@@����*microcodes���Vlp��Vlz@����4��Vl~�Vl�@@��Vl~�Vl�@@@����+end_of_time��W���W��@����3default_end_of_time��W���W��@��W���W��@@@����*visibility��!X���"X��@����2default_visibility��)X���*X��@��,X���-X��@@@@@��/Q �
�0Y��@@@@��2Q � ��3Y��@@��5Q � ��6Y��@���@�����,simple_world��A[���B[��@��D[���E[��@@@��@@���*nb_players��M[���N[��@��P[���Q[��@@@��@@���2nb_robots_per_team��Y[���Z[� @��\[���][� @@@��@@���(max_hell��e[��f[�	@��h[��i[�	@@@��@@���*max_ground��q[�
�r[�@��t[�
�u[�@@@��@@���(max_tree��}[��~[�@���[���[�@@@��@�����%width���\ &��\ +@���\ &��\ +@@@��������#Ext%Float/random_in_range���\ /��\ H@���\ /��\ H@@@��@���$500.@���\ I��\ M@@@��@���%1000.@���\ N��\ S@@@@���\ /��\ S@@@@���\ "��\ S@@��@�����&height���]W]��]Wc@���]W]��]Wc@@@��������#Ext%Float/random_in_range���]Wf��]W@���]Wf��]W@@@��@���$500.@���]W���]W�@@@��@���%1000.@���]W���]W�@@@@���]Wf��]W�@@@@���]WY��]W�@@��@�����,random_angle���^����^��@���^����^��@@@��@@����"()��^���^��@@��	^���
^��@@@�������%Space.angle_of_float��^���^��@��^���^��@@@��@�������&Random%float��&^���'^��@��)^���*^��@@@��@������"*.��5^���6^��@��8^���9^��@@@��@���"2.@��A^���B^��@@@��@�����%Float"pi��M^���N^��@��P^���Q^��@@@@��S^���T^��@@@@��V^���W^��@@@@��Y^���Z^��@@@��\^���]^��A@@@��_^���`^��@@��@�����/random_position��j_���k_��@��m_���n_��@@@��@@���%space��v_���w_��@��y_���z_��@@@��A�����#aux���`���`�@���`���`�@@@��@@���!n���`���`�@���`���`�@@@�  ��������!=���a��a@���a��a@@@��@����!n���a��a@���a��a@@@��@���!0@���a��a@@@@���a��a@@@������(failwith���a$��a,@���a$��a,@@@��@���	*Impossible to find a free random position.@���a-��aY@@@@���a$��aY@@@@���a��aY@@@��@�����!p���b[e��b[f@���b[e��b[f@@@�  !������#Ext%Float���b[i��b[r@���b[i��b[r@@A���b[i��b[r@@���������/random_in_range��cu~�cu�@��
cu~�cu�@@@��@���"0.@��cu��cu�@@@��@����%width��cu��cu�@�� cu��!cu�@@@@��#cu~�$cu�@@@�������/random_in_range��.cu��/cu�@��1cu��2cu�@@@��@���"0.@��:cu��;cu�@@@��@����&height��Dcu��Ecu�@��Gcu��Hcu�@@@@��Jcu��Kcu�@@@@��Mcu}�Ncu�@@@��Pb[i�Qd��@@@@��Sb[a�Td��@@��������"<>��_e���`e��@��be���ce��@@@��@�������%Space&inside��pe���qe��@��se���te��@@@��@����!p��}e���~e��@���e����e��@@@��@��@@�@���e����e��@@@����$true���e����e��@@���e����e��@@@���e����e��@@@��@����%space���e����e��@���e����e��@@@@���e����e��@@@��@����$None���e����e��@@���e����e��@@@@���e����e��@@@������#aux���e����e��@���e����e��@@@��@������!-���e� ��e�@���e� ��e�@@@��@����!n���e����e��@���e����e��@@@��@���!1@���e���e�@@@@���e����e�@@@@���e����e�@@@�����!p���e�
��e�@���e�
��e�@@@���e����e�@@@�� b[a�e�@@@��a�e�@@@��`��e�A@@@��	`� �
e�@@������#aux��g�g@��g�g@@@��@���"10@��g� g@@@@��"g�#g@@@��%`� �&g@@@��(_���)gA@@@��+_���,g@@��@�����+random_size��6i#)�7i#4@��9i#)�:i#4@@@��@@����=��Bi#5�Ci#7@@��Ei#5�Fi#7@@@��������#Ext%Float/random_in_range��Sj:>�Tj:W@��Vj:>�Wj:W@@@��@���$100.@��_j:X�`j:\@@@��@������"/.��kj:d�lj:f@��nj:d�oj:f@@@��@����%width��xj:^�yj:c@��{j:^�|j:c@@@��@���#10.@���j:g��j:j@@@@���j:]��j:k@@@@���j:>��j:k@@@���i#5��j:kA@@@���i#%��j:k@@��A�����)make_hell���lq{��lq�@���lq{��lq�@@@��@@���%space���lq���lq�@���lq���lq�@@@��@�����!s���m����m��@���m����m��@@@�������%Space&square���m����m��@���m����m��@@@��@������/random_position���m����m��@���m����m��@@@��@����%space���m����m��@���m����m��@@@@���m����m��@@@��@������+random_size���m����m��@���m����m��@@@��@��������m����m��@@��m���m��@@@@��m���m��@@@��@����$Hell��m���m��@@��m���m��@@@@��m���m��@@@@��m���m��@@���������%Space0polygon_overlaps��%n���&n��@��(n���)n��@@@��@����!s��2n���3n��@��5n���6n��@@@��@����%space��?n���@n��@��Bn���Cn��@@@��@������!=��Nn���On�@��Qn���Rn�@@@��@����$Hell��[n��\n�@@��^n��_n�@@@@��an���bn�	@@@@��dn���en�	@@@������)make_hell��nn��on�@��qn��rn�@@@��@����%space��{n��|n�@��~n��n�@@@@���n���n�@@@��  !�����%Space���o$(��o$-@���o$(��o$-@@A���o$(��o$-@@������%blend���o$/��o$4@���o$/��o$4@@@��@����%space���o$5��o$:@���o$5��o$:@@@��@������'polygon���o$<��o$C@���o$<��o$C@@@��@����!s���o$D��o$E@���o$D��o$E@@@@���o$;��o$F@@@@���o$/��o$F@@@���o$(��o$G@@@���n����o$G@@@���m����o$G@@@���lq���o$GA@@@���lqs��o$G@@��@�����+make_ground���qMS��qM^@���qMS��qM^@@@��@@���%space���qM_��qMd@���qM_��qMd@@@��@�����%ratio��rgo�rgt@��rgo�rgt@@@����&Ground��rgw�rg}@���������#Ext%Float/random_in_range��rg�rg�@�� rg�!rg�@@@��@���#0.5@��)rg��*rg�@@@��@���#1.5@��2rg��3rg�@@@@��5rg~�6rg�@@@��8rgw�9rg�@@@@��;rgk�<rg�@@��@�����!s��Fs���Gs��@��Is���Js��@@@�������%Space&square��Us���Vs��@��Xs���Ys��@@@��@������/random_position��ds���es��@��gs���hs��@@@��@����%space��qs���rs��@��ts���us��@@@@��ws���xs��@@@��@������+random_size���s����s��@���s����s��@@@��@��������s����s��@@���s����s��@@@@���s����s��@@@��@����%ratio���s����s��@���s����s��@@@@���s����s��@@@@���s����s��@@�  !�����%Space���t����t��@���t����t��@@A���t����t��@@������%blend���t����t� @���t����t� @@@��@����%space���t���t�@���t���t�@@@��@������'polygon���t���t�@���t���t�@@@��@����!s���t���t�@���t���t�@@@@���t���t�@@@@���t����t�@@@���t����t�@@@���s����t�@@@���rgk��t�@@@��qM_�t�A@@@��qMO�t�@@��@�����)make_tree��v�v(@��v�v(@@@��@@���%space��v)�v.@��v)�v.@@@��@@�@��$v/�%v0@@@��@�����-tree_position��/w3;�0w3H@��2w3;�3w3H@@@������/random_position��<w3K�=w3Z@��?w3K�@w3Z@@@��@����%space��Iw3[�Jw3`@��Lw3[�Mw3`@@@@��Ow3K�Pw3`@@@@��Rw37�Sw3`@@��@�����(branches��]xdl�^xdt@��`xdl�axdt@@@��������#Ext#Int/random_in_range��nxdw�oxd�@��qxdw�rxd�@@@��@���!2@��zxd��{xd�@@@��@���"20@���xd���xd�@@@@���xdw��xd�@@@@���xdh��xd�@@������-tree_position���y����y��@�������y����y��@���y����y��@@@����(branches���y����y��@�������y����y��@���y����y��@@@@@���y����y��@@@���xdh��y��@@@���w37��y��@@@���v/��y��A@@���v)��y��A@@@���v��y��@@��@�����)make_team���{����{��@���{����{��@@@��@@���%space���{����{��@���{����{��@@@��@@���/team_identifier���{����{��@���{����{��@@@��@�����)spaceship���|����|��@���|����|��@@@������/random_position���|��� |�@��|���|�@@@��@����%space��|��|�@��|��|�@@@@��|���|�@@@@��|���|�@@��@�����*make_robot�� }�!}#@��#}�$}#@@@��@@���"id��,}$�-}&@��/}$�0}&@@@������*make_robot��9~)/�:~)9@��<~)/�=~)9@@@��@����"id��F~):�G~)<@��I~):�J~)<@@@��@����/team_identifier��S~)=�T~)L@��V~)=�W~)L@@@��@����)spaceship��`~)M�a~)V@��c~)M�d~)V@@@��@������,random_angle��o~)X�p~)d@��r~)X�s~)d@@@��@����v��{~)e�|~)g@@��~~)e�~)g@@@@���~)W��~)h@@@@���~)/��~)h@@@���}$��~)hA@@@���}��~)h@@������/team_identifier��� @pv�� @p�@������� @pv�� @p�@��� @pv�� @p�@@@����)spaceship��� @p��� @p�@������� @p��� @p�@��� @p��� @p�@@@����&robots��� A���� A��@��������#Ext#Fun&repeat��� A���� A��@��� A���� A��@@@��@����2nb_robots_per_team��� A���� A��@��� A���� A��@@@��@����*make_robot��� A���� A��@��� A���� A��@@@@��� A���� A��@@@@@��� @pt�� A��@@@���}�� A��@@@���|���� A��@@@���{���� A��A@@���{���� A��A@@@���{���� A��@@��@�����'nb_hell��	 D���	 D��@��	 D���	 D��@@@��������#Ext#Int/random_in_range��	 D���	 D��@��	 D���	 D��@@@��@���!1@��	  D���	! D��@@@��@����(max_hell��	* D� �	+ D�@��	- D� �	. D�@@@@��	0 D���	1 D�@@@@��	3 D���	4 D�@@��@�����%space��	> E�	? E@��	A E�	B E@@@��������#Ext#Fun$iter��	O E�	P E&@��	R E�	S E&@@@��@����'nb_hell��	\ E'�	] E.@��	_ E'�	` E.@@@��@����)make_hell��	i E/�	j E8@��	l E/�	m E8@@@��@�����%Space%empty��	x E9�	y ED@��	{ E9�	| ED@@@@��	~ E�	 ED@@@@��	� E�	� ED@@��@�����*nb_grounds��	� FHN�	� FHX@��	� FHN�	� FHX@@@��������#Ext#Int/random_in_range��	� FH[�	� FHr@��	� FH[�	� FHr@@@��@���!1@��	� FHs�	� FHt@@@��@����*max_ground��	� FHu�	� FH@��	� FHu�	� FH@@@@��	� FH[�	� FH@@@@��	� FHJ�	� FH@@��@�����%space��	� G���	� G��@��	� G���	� G��@@@��������#Ext#Fun$iter��	� G���	� G��@��	� G���	� G��@@@��@����*nb_grounds��	� G���	� G��@��	� G���	� G��@@@��@����+make_ground��	� G���	� G��@��	� G���	� G��@@@��@����%space��	� G���
  G��@��
 G���
 G��@@@@��
 G���
 G��@@@@��
 G���
	 G��@@��@�����(nb_trees��
 H���
 H��@��
 H���
 H��@@@��������#Ext#Int/random_in_range��
$ H���
% H��@��
' H���
( H��@@@��@���!1@��
0 H���
1 H��@@@��@����(max_tree��
: H���
; H��@��
= H���
> H��@@@@��
@ H���
A H��@@@@��
C H���
D H��@@��@�����%trees��
N I���
O I�	 @��
Q I���
R I�	 @@@��������#Ext#Fun&repeat��
_ I�	�
` I�	@��
b I�	�
c I�	@@@��@����(nb_trees��
l I�	�
m I�	@��
o I�	�
p I�	@@@��@������)make_tree��
{ I�	�
| I�	%@��
~ I�	�
 I�	%@@@��@����%space��
� I�	&�
� I�	+@��
� I�	&�
� I�	+@@@@��
� I�	�
� I�	,@@@@��
� I�	�
� I�	,@@@@��
� I���
� I�	,@@��@�����%teams��
� J	0	6�
� J	0	;@��
� J	0	6�
� J	0	;@@@��������#Ext#Fun&repeat��
� J	0	>�
� J	0	L@��
� J	0	>�
� J	0	L@@@��@����*nb_players��
� J	0	M�
� J	0	W@��
� J	0	M�
� J	0	W@@@��@������)make_team��
� J	0	Y�
� J	0	b@��
� J	0	Y�
� J	0	b@@@��@����%space��
� J	0	c�
� J	0	h@��
� J	0	c�
� J	0	h@@@@��
� J	0	X�
� J	0	i@@@@��
� J	0	>�
� J	0	i@@@@��
� J	0	2�
� J	0	i@@������%space��
� K	m	~�
� K	m	�@������
� K	m	~�
� K	m	�@��
� K	m	~�
� K	m	�@@@����%trees�� K	m	�� K	m	�@������ K	m	��	 K	m	�@�� K	m	�� K	m	�@@@����%teams�� K	m	�� K	m	�@������ K	m	�� K	m	�@�� K	m	�� K	m	�@@@@�����'initial��& K	m	q�' K	m	x@��) K	m	q�* K	m	x@@@��, K	m	o�- K	m	�@@@��/ J	0	2�0 K	m	�@@@��2 I���3 K	m	�@@@��5 H���6 K	m	�@@@��8 G���9 K	m	�@@@��; FHJ�< K	m	�@@@��> E�? K	m	�@@@��A D���B K	m	�@@@��D{���E K	m	�@@@��Gv�H K	m	�@@@��JqMO�K K	m	�@@@��Mlqs�N K	m	�@@@��Pi#%�Q K	m	�@@@��S_���T K	m	�@@@��V^���W K	m	�@@@��Y]WY�Z K	m	�@@@��\\ "�] K	m	�@@@��_[��` K	m	�A@@��b[�
�c K	m	�A@@��e[��f K	m	�A@@��h[���i K	m	�A@@��k[���l K	m	�A@@@��n[���o K	m	�@@��q[���r K	m	�@���@�����&output��} M	�	��~ M	�	�@��� M	�	��� M	�	�@@@��@@���%world��� M	�	��� M	�	�@��� M	�	��� M	�	�@@@������"|>��� N	�	��� N	�	�@��� N	�	��� N	�	�@@@��@������"|>��� N	�	��� N	�	�@��� N	�	��� N	�	�@@@��@������)to_yojson��� N	�	��� N	�	�@��� N	�	��� N	�	�@@@��@����%world��� N	�	��� N	�	�@��� N	�	��� N	�	�@@@@��� N	�	��� N	�	�@@@��@������&Yojson$Safe0pretty_to_string��� N	�	��� N	�	�@��� N	�	��� N	�	�@@@@��� N	�	��� N	�	�@@@��@������-output_string��� N	�	��� N	�	�@��� N	�	��� N	�	�@@@��@����&stdout��� N	�	��� N	�	�@��� N	�	��� N	�	�@@@@��� N	�	��� N	�	�@@@@��� N	�	��� N	�	�@@@��  M	�	�� N	�	�A@@@�� M	�	�� N	�	�@@�� M	�	�� N	�	�@���@�����(generate�� P	�	�� P	�	�@�� P	�	�� P	�	�@@@��@@���)visualize�� Q
 
� Q
 
@��! Q
 
�" Q
 
@@@��@@���*nb_players��* Q
 
�+ Q
 
@��- Q
 
�. Q
 
@@@��@@���3nb_robots_per_teams��6 Q
 
�7 Q
 
.@��9 Q
 
�: Q
 
.@@@��@@���(max_hell��B R
/
5�C R
/
=@��E R
/
5�F R
/
=@@@��@@���*max_ground��N R
/
>�O R
/
H@��Q R
/
>�R R
/
H@@@��@@���(max_tree��Z R
/
I�[ R
/
Q@��] R
/
I�^ R
/
Q@@@��@�����%world��h S
T
Z�i S
T
_@��k S
T
Z�l S
T
_@@@������,simple_world��u T
b
f�v T
b
r@��x T
b
f�y T
b
r@@@��@����*nb_players��� T
b
s�� T
b
}@��� T
b
s�� T
b
}@@@��@����3nb_robots_per_teams��� T
b
~�� T
b
�@��� T
b
~�� T
b
�@@@��@����(max_hell��� T
b
��� T
b
�@��� T
b
��� T
b
�@@@��@����*max_ground��� T
b
��� T
b
�@��� T
b
��� T
b
�@@@��@����(max_tree��� T
b
��� T
b
�@��� T
b
��� T
b
�@@@@��� T
b
f�� T
b
�@@@@��� S
T
V�� T
b
�@@�  ������)visualize��� V
�
��� V
�
�@��� V
�
��� V
�
�@@@�  !�����*Visualizer��� V
�
��� V
�
�@��� V
�
��� V
�
�@@A��� V
�
��� V
�
�@@�  ������$show��� V
�
��� V
�
�@��� V
�
��� V
�
�@@@��@����%world��� V
�
��� V
�
�@��� V
�
��� V
�
�@@@@��� V
�
��� V
�
�@@@������%pause�� V
�
��	 V
�
�@�� V
�
�� V
�
�@@@��@������ V
�
�� V
�
�@@�� V
�
�� V
�
�@@@@�� V
�
�� V
�
�@@@�� V
�
�� V
�
�@@@��  V
�
��! V
�
�@@@@��# V
�
��$ V
�
�@@@������&output��- W
�
��. W
�
�@��0 W
�
��1 W
�
�@@@��@����%world��: W
�
��; W
�
�@��= W
�
��> W
�
�@@@@��@ W
�
��A W
�
�@@@��C V
�
��D W
�
�@@@��F S
T
V�G W
�
�@@@��I R
/
I�J W
�
�A@@��L R
/
>�M W
�
�A@@��O R
/
5�P W
�
�A@@��R Q
 
�S W
�
�A@@��U Q
 
�V W
�
�A@@��X Q
 
�Y W
�
�A@@@��[ P	�	��\ W
�
�@@��^ P	�	��_ W
�
�@���@�����2visualization_flag��j Y
��k Y
�@��m Y
��n Y
�@@@�  !�����(Cmdliner��x Y
��y Y
�@��{ Y
��| Y
�@@A��~ Y
�� Y
�@@�  !�����#Arg��� Y
� �� Y
�#@��� Y
� �� Y
�#@@A��� Y
� �� Y
�#@@������!&��� Z&.�� Z&/@��� Z&.�� Z&/@@@��@����%value��� Z&(�� Z&-@��� Z&(�� Z&-@@@��@��������� Z&5�� Z&6@��� Z&5�� Z&6@@@��@����$flag��� Z&0�� Z&4@��� Z&0�� Z&4@@@��@������$info��� Z&7�� Z&;@��� Z&7�� Z&;@@@��@����"::��� Z&=�� Z&AA�������!v@��� Z&=�� Z&@@@@�����"[]��� Z&@�� Z&AA@��� Z&@�� Z&AA@@@��� Z&=�� Z&AA@@��� Z&<�� Z&A@@@���#doc���=Visualize the generated world@�� [BI� [Bh@@@@�� Z&7�	 [Bh@@@@�� Z&0� [Bh@@@@�� Z&(� [Bh@@@�� Y
� � [Bi@@@�� Y
�� \jk@@@@�� Y
�
�� \jk@@�� Y
�
�� \jk@���@�����*nb_players��& ^mq�' ^m{@��) ^mq�* ^m{@@@�  !�����(Cmdliner��4 ^m~�5 ^m�@��7 ^m~�8 ^m�@@A��: ^m~�; ^m�@@�  !�����#Arg��E ^m��F ^m�@��H ^m��I ^m�@@A��K ^m��L ^m�@@���������T _���U _��@��W _���X _��@@@��@����%value��a _���b _��@��d _���e _��@@@��@������װ�o _���p _��@��r _���s _��@@@��@������#opt��~ _��� _��@��� _���� _��@@@��@����#int��� _���� _��@��� _���� _��@@@��@���!1@��� _���� _��@@@@��� _���� _��@@@��@������$info��� _���� _��@��� _���� _��@@@��@����ְ�� _���� _��A�������!p@��� _���� _��@@@�����հ�� _���� _��A@��� _���� _��A@@@��� _���� _��A@@��� _���� _��@@@���$docv���)NBPLAYERS@��� `���� `��@@@���#doc���7Handle $(docv) players.@��� a���� a��@@@@��� _���� a��@@@@��� _���� a��@@@@��� _���� a��@@@��� ^m��� b��@@@��� ^m~�� b��@@@@��� ^mm�� b��@@��� ^mm�� b��@���@�����)nb_robots�� d��� d��@�� d���	 d��@@@�  !�����(Cmdliner�� d��� d� @�� d��� d� @@A�� d��� d� @@�  !�����#Arg��$ d��% d�@��' d��( d�@@A��* d��+ d�@@���������3 e�4 e@��6 e�7 e@@@��@����%value��@ e
�A e@��C e
�D e@@@��@���������N e�O e@��Q e�R e@@@��@������#opt��] e�^ e@��` e�a e@@@��@����#int��j e�k e@��m e�n e@@@��@���!1@��v e�w e@@@@��y e�z e@@@��@������$info��� e�� e"@��� e�� e"@@@��@�������� e$�� e(A�������!r@��� e$�� e'@@@��������� e'�� e(A@��� e'�� e(A@@@��� e$�� e(A@@��� e#�� e(@@@���$docv���(NBROBOTS@��� f)1�� f);@@@���#doc���	!Handle $(docv) robots per player.@��� g<C�� g<f@@@@��� e�� g<f@@@@��� e�� g<f@@@@��� e
�� g<f@@@��� d��� hgh@@@��� d���� hgi@@@@��� d���� hgi@@��� d���� hgi@���@�����(max_hell��� jko�� jkw@��� jko�� jkw@@@�  !�����(Cmdliner��� jkz�� jk�@��� jkz�� jk�@@A��� jkz�� jk�@@�  !�����#Arg�� jk�� jk�@�� jk�� jk�@@A��	 jk��
 jk�@@������z�� k��� k��@�� k��� k��@@@��@����%value�� k���  k��@��" k���# k��@@@��@���������- k���. k��@��0 k���1 k��@@@��@������#opt��< k���= k��@��? k���@ k��@@@��@����#int��I k���J k��@��L k���M k��@@@��@���!1@��U k���V k��@@@@��X k���Y k��@@@��@������$info��d k���e k��@��g k���h k��@@@��@�������p k���q k��A�������!h@��{ k���| k��@@@��������� k���� k��A@��� k���� k��A@@@��� k���� k��A@@��� k���� k��@@@���$docv���'MAXHELL@��� l���� l��@@@���#doc���	%Use a maximum of $(docv) hell blocks.@��� m���� m��@@@@��� k���� m��@@@@��� k���� m��@@@@��� k���� m��@@@��� jk��� n��@@@��� jkz�� n��@@@@��� jkk�� n��@@��� jkk�� n��@���@�����*max_ground��� p���� p��@��� p���� p��@@@�  !�����(Cmdliner��� p��� p�	@��� p��� p�	@@A��� p��� p�	@@�  !�����#Arg��� p��� p�@��� p��� p�@@A��� p��� p�@@������Y��� q�� q@��� q�� q@@@��@����%value��� q�� q@�� q� q@@@��@������t�� q%� q&@�� q%� q&@@@��@������#opt�� q� q@�� q� q@@@��@����#int��( q�) q"@��+ q�, q"@@@��@���!1@��4 q#�5 q$@@@@��7 q�8 q$@@@��@������$info��C q'�D q+@��F q'�G q+@@@��@����s��O q-�P q1A�������!g@��Z q-�[ q0@@@�����r��b q0�c q1A@��e q0�f q1A@@@��h q-�i q1A@@��k q,�l q1@@@���$docv���)MAXGROUND@��v r2:�w r2E@@@���#doc���	'Use a maximum of $(docv) ground blocks.@��� sFM�� sFv@@@@��� q'�� sFv@@@@��� q�� sFv@@@@��� q�� sFv@@@��� p��� twx@@@��� p��� twy@@@@��� p���� twy@@��� p���� twy@���@�����(max_tree��� v{�� v{�@��� v{�� v{�@@@�  !�����(Cmdliner��� v{��� v{�@��� v{��� v{�@@A��� v{��� v{�@@�  !�����#Arg��� v{��� v{�@��� v{��� v{�@@A��� v{��� v{�@@������8��� w���� w��@��� w���� w��@@@��@����%value��� w���� w��@��� w���� w��@@@��@������S��� w���� w��@��� w���� w��@@@��@������#opt��� w���� w��@��� w���� w��@@@��@����#int�� w��� w��@��
 w��� w��@@@��@���!1@�� w��� w��@@@@�� w��� w��@@@��@������$info��" w���# w��@��% w���& w��@@@��@����R��. w���/ w��A�������!t@��9 w���: w��@@@�����Q��A w���B w��A@��D w���E w��A@@@��G w���H w��A@@��J w���K w��@@@���$docv���'MAXTREE@��U x���V x��@@@���#doc���?Use a maximum of $(docv) trees.@��` y���a y��@@@@��c w���d y��@@@@��f w���g y��@@@@��i w���j y��@@@��l v{��m z��@@@��o v{��p z��@@@@��r v{{�s z��@@��u v{{�v z��@���@�����#cmd��� |���� |�@��� |���� |�@@@�  !�����(Cmdliner��� |��� |�@��� |��� |�@@A��� |��� |�@@��@�����#doc��� }�� }@��� }�� }@@@���8Generate a random world.@��� }�� }7@@@@��� }�� }7@@��@�����%exits��� ~;A�� ~;F@��� ~;A�� ~;F@@@�����$Term-default_exits��� ~;I�� ~;[@��� ~;I�� ~;[@@@@��� ~;=�� ~;[@@����  !�����$Term��� _a�� _e@��� _a�� _e@@A��� _a�� _e@@������!$��� ����� ���@��� ����� ���@@@��@������!$��� ����� ���@��� ����� ���@@@��@������!$�� ���� ���@��
 ���� ���@@@��@������!$�� ���� ���@�� ���� ���@@@��@������!$��% ����& ���@��( ����) ���@@@��@������!$��4 _v�5 _w@��7 _v�8 _w@@@��@������%const��C _g�D _l@��F _g�G _l@@@��@����(generate��P _m�Q _u@��S _m�T _u@@@@��V _g�W _u@@@��@����2visualization_flag��` _x�a _�@��c _x�d _�@@@@��f _g�g _�@@@��@����*nb_players��p ����q ���@��s ����t ���@@@@��v _g�w ���@@@��@����)nb_robots��� ����� ���@��� ����� ���@@@@��� _g�� ���@@@��@����(max_hell��� ����� ���@��� ����� ���@@@@��� _g�� ���@@@��@����*max_ground��� ����� ���@��� ����� ���@@@@��� _g�� ���@@@��@����(max_tree��� ����� ���@��� ����� ���@@@@��� _g�� ���@@@��� _a�� ���@@@��������$Term$info��� ����� ���@��� ����� ���@@@��@���(generate@��� ����� ���@@@���#doc������� ����� ���@��� ����� ���@@@���%exits������� ����� ���@��� ����� ���@@@@��� ����� ���@@@@��� _a�� ���@@@��� ~;=�� ���@@@��� }�� ���@@@��� |��� ���@@@@��  |��� ���@@�� |��� ���@���@�������� ���� ���@@�� ���� ���@@@��������3Ppx_inline_test_lib'Runtime)unset_lib��  ����! ���@��# ����$ ���@@@��@���@��+ ����, ���@@@@��. ����/ ���@@@@��1 ����2 ���@@��4 ����5 ���@@