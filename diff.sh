
iecho "---------------------------------------------------------------------------------------------------------------"
echo "---------------------------------------------------------------------------------------------------------------"
echo " ENTITIES  "
sort /workspace/ScalaClean/testProjects/deadCodeProject1/target/scala-2.12/classes/META-INF/ScalaClean/scalaclean-elements.csv | grep -v Import > /tmp/newElements.csv
sort /Users/rorygraves/Downloads/temp3/testProjects/deadCodeProject1/src/main/scala/scalaclean-elements.csv | grep -v Import > /tmp/oldElements.csv
diff /tmp/newElements.csv /tmp/oldElements.csv


echo "---------------------------------------------------------------------------------------------------------------"
echo "---------------------------------------------------------------------------------------------------------------"
sort /workspace/ScalaClean/testProjects/deadCodeProject1/target/scala-2.12/classes/META-INF/ScalaClean/scalaclean-relationships.csv | grep -v Import > /tmp/newRels.csv
sort /Users/rorygraves/Downloads/temp3/testProjects/deadCodeProject1/src/main/scala/scalaclean-relationships.csv | grep -v Import > /tmp/oldRels.csv
echo " RELS  " 
diff /tmp/newRels.csv /tmp/oldRels.csv
