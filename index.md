---
layout: default
title: Algorithms
---
{% assign languages = "java, erlang" | split: ", " %}
<ul class="list-group">
  {% for language in languages %}
    <li class="list-group-item" active>{{ language | capitalize }}</li>
    {% for page in site.pages %}
      {% if page.url contains "exercises" and page.language == language %}
        <li class="list-group-item">
          <a href="{{ page.url | relative_url }}">{{ page.title }}</a>
        </li>
      {% endif %}
    {% endfor %}
  {% endfor %}
</ul>
