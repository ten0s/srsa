---
layout: default
title: Spaced Repetition System (Algorithms)
---

{% assign languages = "Java, Erlang" | split: ", " %}

<ul class="list-group">

  {% for language in languages %}

    <li class="list-group-item" active>{{ language }}</li>

    {% for page in site.pages %}

      {% if page.url contains "exercises" and page.url contains language %}

        <li class="list-group-item">
          <a href="{{ page.url | relative_url }}">{{ page.title }}</a>
        </li>

      {% endif %}

    {% endfor %}

  {% endfor %}

</ul>
