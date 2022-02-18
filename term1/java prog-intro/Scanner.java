import java.util.*;
import java.io.*;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.BufferedInputStream;

public class Scanner {
	private Reader input;
	private String prev;
	private String data;
	private int currentLine = 0;
	private int used = 0;
	private int current = 0;
	
	public Scanner(InputStream stream) throws IOException {
		input = new InputStreamReader(stream);
	}
	
	public Scanner(InputStreamReader stream) throws IOException {
		input = stream;
	}
	
	public Scanner(String str) throws IOException {
		input = new StringReader(str);
	}
	
	public boolean hasNextLine() throws IOException {
		if (current == -1) {
			return false;
		}
		return true;
	}
	
	public boolean hasNextWord() throws IOException {
		if (used == 0) {
			hiddenNextWord();
			used = 1;
		}
		if (data.length() == 0) {
			return false;
		}
		return true;
	}
	
	public boolean hasNextInt() throws IOException {
		if (used == 0) {
			hiddenNextInt();
			used = 1;
		}
		if (data.length() == 0) {
			return false;
		}
		return true;
	}
	
	public String nextLine() throws IOException {
		hiddenNextLine();
		return data;
	}
	
	public String nextWord() throws IOException {
		hiddenNextWord();
		return prev;
	}
	
	public int nextInt() throws IOException {
		hiddenNextInt();
		return Integer.parseInt(prev);
	}
	
	public int currentLine() {
		return currentLine;
	}
	
	private void hiddenNextLine() throws IOException {
		StringBuilder str = new StringBuilder();
		nextChar();
		
		while(current != -1) {
			if ((char)current == '\n') {
				break;
			} else {
				str.append((char)current);
			}
			nextChar();
		}
		update(str);
	}
	
	private void hiddenNextWord() throws IOException {
		StringBuilder str = new StringBuilder();
		nextChar();
		
		while(current != -1) {
			if ((char)current == '\n') {
				currentLine++;
			}
			if (!Character.isLetter((char)current) &&
				Character.getType((char)current) != Character.DASH_PUNCTUATION &&
				(char)current != '\'') {
				if (str.length() > 0) {
					break;
				}
			} else {
				str.append((char)current);
			}
			nextChar();
		}
		update(str);
	}
	
	private void hiddenNextInt() throws IOException {
		StringBuilder str = new StringBuilder();
		nextChar();
		
		while(current != -1) {
			if ((char)current == '\n') {
				currentLine++;
			}
			if (!Character.isDigit((char)current) && (char)current != '-') {
				if (str.length() > 0) {
					break;
				}
			} else {
				if ((char)current == '-') {
					if (str.length() == 0) {
						str.append((char)current);
					} else {
						break;
					}
				} else {
					str.append((char)current);
				}
			}
			nextChar();
		}
		update(str);
	}
	
	private void nextChar() throws IOException {
		current = input.read();
	}
	
	private void update(StringBuilder str) {
		prev = data;
		data = str.toString();
	}
	
	public void close() throws IOException {
		input.close();
	}
}