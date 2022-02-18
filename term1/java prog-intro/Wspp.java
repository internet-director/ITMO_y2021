import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.LinkedHashMap;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;

public class Wspp {
	public static void main(String[] args) throws IOException {
		StringBuilder str = new StringBuilder("");

		try { 
			Scanner in = new Scanner(new InputStreamReader(new FileInputStream(args[0]), "utf8"));
			BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), "utf8"));

			Map<String, Data> words = new LinkedHashMap<String, Data>();
			int count = 0;
			int line = 0;
			
			while (in.hasNextWord()) {
				String word = in.nextWord().toLowerCase();
				if (words.containsKey(word)) {
					Data t = words.get(word);
					t.add(" " + (++count));
					words.put(word, t);
				} else {
					Data p = new Data();
					p.add("" + (++count));
					words.put(word, p);
				}
			}

			for (String i : words.keySet()) {
				out.write(i + " " + words.get(i).currentPosition() + " " + words.get(i).getData());
				//System.err.println(i + " " + words.get(i).currentPosition() + " " + words.get(i).getData());
				out.write("\n");
			}
			
			in.close();
			out.close();
			
		} catch (FileNotFoundException e) {
			System.err.println("File not found");
		} catch (IOException e) {
			System.err.println("File error");
		}
	}
}