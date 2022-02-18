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

class Data {
	private int position = 0;
	private StringBuilder str = new StringBuilder();
	
	public void add(String addData) {
		this.position++;
		str.append(addData);
	}
	
	public int currentPosition() {
		return this.position;
	}
	
	public String getData() {
		return str.toString();
	}
}