package markup;

import java.util.List;

public class Paragraph implements Container {
    protected final List<Container> list;

    public Paragraph(final List<Container> lc) {
        list = lc;
    }

    public void toMarkdown(StringBuilder sb) {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).toMarkdown(sb);
        }
    }
    public void toBBCode(StringBuilder sb) {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).toBBCode(sb);
        }
    }
}