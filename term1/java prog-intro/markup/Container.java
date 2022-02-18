package markup;

import java.lang.StringBuilder;

public interface Container {
    public void toMarkdown(StringBuilder sb);
    public void toBBCode(StringBuilder sb);
}