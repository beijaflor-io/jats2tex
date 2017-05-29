# -*- coding: utf-8 -*-
import scrapy


class ArticlesSpider(scrapy.Spider):
    name = "articles"
    allowed_domains = ["www.scielo.org", "www.scielo.org.bo"]
    start_urls = ['http://www.scielo.org', "http://www.scielo.org.bo/scielo.php?script=sci_arttext&pid=S1683-07892012000100003&lang=pt"]

    def parse(self, response):
        xml_link = response.css('#toolBox > div:nth-child(3) > ul > li:nth-child(2) > a::attr(href)').extract_first()
        if xml_link:
            yield { 'link': xml_link }

        for link in response.css('a::attr(href)'):
            if link:
                yield scrapy.Request(response.urljoin(link.extract()), callback=self.parse)

