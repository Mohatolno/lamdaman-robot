Caml1999M025����            3src/lambdaDriver.ml����    �  �  Z�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���-ppxlib_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����o�@�@@@@�@@@�@�������*ocaml.text���@@ ���@@ �A�������
  @

   Ce module implémente les deux points d'entrée de l'exécutable :

   - "lambda man" implémente un robot. Dans ce cas, le programme se
     met en attente d'une interaction avec le serveur. Si on lui passe
     l'option "-n" suivie d'un entier N alors N robots seront pris en
     chargent par le client.

   - "lambda server" implémente le serveur de jeu.

      Il attend l'argument "-w" suivi d'un nom de fichier JSON
      décrivant le monde au format décrit dans le module World.

      Par défaut, il utilise la commande "lambda man" pour commander tous
      les robots de toutes les équipes.

      On peut aussi lui préciser une liste de commandes permettant de
      lancer des "lambda men". Dans ce cas,  il faut autant de commandes
      que d'équipes déclarées dans le fichier de description du monde.

@��3src/lambdaDriver.mlA@@�VCE@@@@��A@@�VCE@@���@@ ���@@ �A��
A@@�VCE@��������)LambdaLib��XGL�XGU@��XGG�XGU@@A��XGG�XGU@@��XGG� XGU@��������(Cmdliner��+YV[�,YVc@��.YVV�/YVc@@A��1YVV�2YVc@@��4YVV�5YVc@���@�����+default_cmd��@[ei�A[et@��C[ei�D[et@@@�����)LambdaMan#cmd��M[ew�N[e�@��P[ew�Q[e�@@@@��S[ee�T[e�@@��V[ee�W[e�@���@�����$cmds��b]���c]��@��e]���f]��@@@����"::��m]���n]��A���������)LambdaMan#cmd��{]���|]��@��~]���]��@@@��������]����]��A���������,LambdaServer#cmd���]����]��@���]����]��@@@�����3���]����]��A���������.WorldGenerator#cmd���]����]��@���]����]��@@@�����"[]���]����]��A@���]����]��A@@@���]����]��A@@���]����]��A@@@���]����]��A@@���]����]��A@@@���]����]��A@@���]����]��@@@@���]����]��@@���]����]��@���@�����$main���_����_��@���_����_��@@@�  !�����$Term���_����_��@���_����_��@@A���_����_��@@������"@@���_����_��@��_���_��@@@��@����$exit��_���_��@��_���_��@@@��@������+eval_choice��_���_��@��_���_��@@@��@����+default_cmd��'_���(_��@��*_���+_��@@@��@����$cmds��4_���5_��@��7_���8_��@@@@��:_���;_��@@@@��=_���>_��@@@��@_���A_� @@@@��C_���D_� @@��F_���G_� @@