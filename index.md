---
layout: default
title: Exercises
---
{% comment %}
   Build languages array
{% endcomment %}

{% assign languages = "" %}
{% for page in site.pages %}
   {% if page.url contains "exercises" %}
      {% assign languages = languages | append: " " | append: page.language %}
   {% endif %}
{% endfor %}
{% assign languages = languages | split: " " | sort | uniq %}

{% comment %}
   Group exercises by array
{% endcomment %}

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
