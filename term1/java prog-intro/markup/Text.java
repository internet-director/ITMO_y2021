package markup;

import java.util.*;

public class Text implements Container {
    public String text;

    public Text(final String cont) {
        this.text = new String(cont);
    }

    public void toMarkdown(StringBuilder sb) {
        sb.append(text);
    }
    public void toBBCode(StringBuilder sb) {
        sb.append(text);
    }
}