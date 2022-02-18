import java.util.*;
import java.io.*;

public class WordStatWords {
	public static void main(String[] args) throws IOException {
		StringBuilder str = new StringBuilder("");

		try(BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), "utf8"));
		BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), "utf8"));) { 
			int charCode;
			char[] buffer = new char[1024];
			while ((charCode = in.read(buffer, 0, 1024)) != -1) {
				str.append(buffer, 0, charCode);
			}
			
			List<String> words = getWords(str);	
			List<String> nRep = getNoRepeat(words);
			Collections.sort(nRep);
			int[] num = new int[nRep.size()];
						
			for(int j = 0; j < nRep.size(); j++) {
				for(int i = 0; i < words.size(); i++) {
					if(nRep.get(j).equals(words.get(i))) {
						num[j]++;
					}
				}
			}
			
			for(int i = 0; i < nRep.size(); i++) {
				out.write(nRep.get(i) + " " + num[i] + "\n");
			}
			
		} catch(FileNotFoundException e) {
			System.err.println("File not found");
		} catch(IOException e) {
			
		}
	}
	
	static private List<String> getWords(StringBuilder str) {
		str.append(" ");
		StringBuilder word = new StringBuilder("");
		List<String> words = new ArrayList<String>();
		
		for(int i = 0; i < str.length(); i++) {
			if(Character.isLetter(str.charAt(i)) || Character.getType(str.charAt(i)) == Character.DASH_PUNCTUATION || str.charAt(i) == '\'') {
				word.append(str.charAt(i));
			} else {
				if(word.length() > 0) {
					words.add(word.toString().toLowerCase());
					word.setLength(0);
				}
			}
		}
		return words;
	}
	
	static private List<String> getNoRepeat(List<String> words) {
		ArrayList<String> nRep = new ArrayList<String>();
		nRep.add(words.get(0));
		
		for(int i = 0; i < words.size(); i++) {
			if(!checkRepeat(nRep, words.get(i)))
				nRep.add(words.get(i));
			}
		return nRep;
	}
  
	static private boolean checkRepeat(List<String> words, String str) {
		for(int i = 0; i < words.size(); i++)
			if(words.get(i).equals(str))
				return true;
		return false;
	}
}