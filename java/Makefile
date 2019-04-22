all: jar

jar:
	javac ld41/Minesweeper.java ld41/Ld41.java
	jar -cvfm Ld41.jar manifest.txt ld41/Minesweeper.class ld41/Ld41.class

clean:
	rm -f ld41/*.class
