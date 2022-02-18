package md2html;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

class converter {
	private StringBuilder str = new StringBuilder();
	private final String htmlcode[] = {"code", "em", "em", "strong", "strong", "s", "var"};
	private final String mdcode[] = {"`", "*", "_", "**", "__", "--", "%"};
	private final String fixcode[] = {"*", "_"};
	private final char repcode[] = {'<', '>', '&'};
	private final String tocode[] = {"&lt;", "&gt;", "&amp;"};

	public converter(StringBuilder line) {
		paragraph(line);
		shield();	
		
		for (int i = 0; i < fixcode.length; i++) {
			fixLine(fixcode[i]);
		}
		for (int i = 0; i < htmlcode.length; i++) {
			html(mdcode[i], htmlcode[i]);
		}
	}
	
	public String toStr() {
		return str.toString();
	}
	
	private void html(final String code, final String replace) {
		boolean isCode = false;
		int codePosition = 0; 
		StringBuilder result = new StringBuilder();
		
		if (code.length() == 1) {
			for (int i = 0; i < str.length() - 1; i++) {
				if (str.charAt(i) == code.charAt(0)) {
					if (str.charAt(i + 1) != code.charAt(0)) {
						replace(result, replace, codePosition, i, isCode);
						isCode = !isCode;
						codePosition = i + 1;
					} else {
						i += 2;
					}
				}
			}
		} else {
			for (int i = 0; i < str.length() - 1; i++) {
				if (str.charAt(i) == code.charAt(0)  && str.charAt(i + 1) == code.charAt(1)) {
					replace(result, replace, codePosition, i, isCode);
					isCode = !isCode;
					codePosition = i + 2;
				}
			}
		}

		result.append(str.substring(codePosition, str.length()));
		str = result;
	}
	
	private void replace(StringBuilder result, String code, int start, int end, boolean isCode) {
		if (str.charAt(end - 1) == '\\') {
			end--;
		}
		String sub = str.substring(start, end);
		result.append(sub);
		if (str.charAt(end) == '\\') {
			result.append(str.charAt(end + 1));
		} else {
			result.append("<");
			if (isCode) {
				result.append("/");
			}
			result.append(code);
			result.append(">");
		}
	}
	
	private void shield() {
		StringBuilder result = new StringBuilder();
		int position = 4;
		result.append(str.substring(0, 4));
		
		for (int i = 4; i < str.length() - 5; i++) {
			for (int j = 0; j < repcode.length; j++) {
				if (str.charAt(i) == repcode[j]) {
					result.append(str.substring(position, i));
					result.append(tocode[j]);
					position = i + 1;
				}
			}
		}
		
		result.append(str.substring(position, str.length()));
		str = result;
	}
	
	private void fixLine(String code) {
		int helicopterCount = 0;
		int helicopterPosition = 0;
		
		for (int i = 4; i < str.length() - 5; i++) {
			if (str.charAt(i) == code.charAt(0) && str.charAt(i - 1) != '\\') {
				helicopterCount++;
				helicopterPosition = i;
			}
		}
				
		if (helicopterCount == 1) {
			StringBuilder dop = new StringBuilder();
			dop.append(str.substring(0, helicopterPosition));
			dop.append('\\' + code);
			dop.append(str.substring(helicopterPosition + 1, str.length()));
			str = dop;
		}
	}
	
	private void paragraph(StringBuilder line) {
		if (line.charAt(0) != '#') {
			str.append("<p>");
			str.append(line.substring(0, line.length() - 1));
			str.append("</p>");
		} else {
			int i = 0;
			int space = 0;
			boolean next = true;
			for (i = 0; i < line.length(); i++) {
				if (line.charAt(i) != '#') {
					for (int j = i; j < line.length(); j++) {
						if (!Character.isWhitespace(line.charAt(j))) {
							break;
						}
						space++;
						i++;
					}
					break;
				}
			}

			if (space > 0) {
				str.append("<h" + (i - 1) + ">");
				str.append(line.substring(i, line.length() - 1));
				str.append("</h" + (i - 1) + ">");
			} else {
				str.append("<p>");
				str.append(line.substring(0, line.length() - 1));
				str.append("</p>");
			}
		}
	}
}

public class Md2Html {
    public static void main(String[] args) {
		try(BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), "utf8"));
		BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), "utf8"));) {
			String line = in.readLine();
			while (line != null) {
				StringBuilder currentLine = new StringBuilder();
				int skip = 0;
				while (line != null) {
					if (!line.isEmpty()) {
						currentLine.append(line + '\n');
						skip = 0;
					} else if (currentLine.length() > 0) {
						skip++;
					}
					if (skip > 0) {
						break;
					}
					line = in.readLine();
				}
				converter conv = new converter(currentLine);
				
				out.write(conv.toStr() + '\n');
				line = in.readLine();
			}		
		} catch(FileNotFoundException e) {
			System.err.println("File not found " + e.getMessage());
		} catch(IOException e) {
			System.err.println("I/O error: " + e.getMessage());			
		}
	}
}