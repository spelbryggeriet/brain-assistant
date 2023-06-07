use std::borrow::Cow;

use ratatui::{
    layout::Rect,
    text::{Line, Span, Text},
};

pub struct TextComposer<'a> {
    text: Text<'a>,
    dimensions: Rect,
    first_index: usize,
    last_index: usize,
}

impl<'a> TextComposer<'a> {
    pub fn new(dimensions: Rect) -> Self {
        Self {
            text: Text {
                lines: vec![Line::default(); dimensions.height as usize],
            },
            dimensions,
            first_index: 0,
            last_index: dimensions.height as usize,
        }
    }

    pub fn extend_text(&mut self, text: Text<'a>) -> bool {
        self.wrap_lines(text)
            .lines
            .into_iter()
            .take(self.available_lines())
            .for_each(|line| {
                self.text.lines[self.first_index] = line;
                self.first_index += 1;
            });
        self.available_lines() > 0
    }

    pub fn extend_text_from_back(&mut self, text: Text<'a>) -> bool {
        self.wrap_lines(text)
            .lines
            .into_iter()
            .rev()
            .take(self.available_lines())
            .for_each(|line| {
                self.last_index -= 1;
                self.text.lines[self.last_index] = line;
            });
        self.available_lines() > 0
    }

    pub fn available_lines(&self) -> usize {
        self.last_index - self.first_index
    }

    pub fn finish(mut self) -> Text<'a> {
        if self.first_index == 0 {
            let diff = (self.dimensions.height as usize)
                .saturating_sub(self.dimensions.height as usize - self.last_index);
            if diff > 0 {
                self.text.lines.rotate_left(diff);
            }
        }
        self.text
    }

    fn wrap_line(&self, line: Line<'a>) -> Text<'a> {
        if self.dimensions.width == 0 {
            return Text::from(line);
        }

        let mut text = Text::from(Line::default());
        for mut span in line.spans.into_iter() {
            let last_line = text.lines.last_mut().unwrap();
            let mut span_width = span.width();
            let mut left_on_last_line = self.dimensions.width as usize - last_line.width();

            if span_width <= left_on_last_line {
                last_line.spans.push(span);
                continue;
            }

            if left_on_last_line == 0 {
                text.lines.push(Line::default());
                left_on_last_line = self.dimensions.width as usize;
            }

            while span_width > left_on_last_line {
                let line = text.lines.last_mut().unwrap();

                let span_split = Span {
                    content: Cow::Owned(span.content[..left_on_last_line].to_owned()),
                    ..span
                };
                span.content = Cow::Owned(span.content[left_on_last_line..].to_owned());
                span_width = span.width();

                line.spans.push(span_split);
                text.lines.push(Line::default());
                left_on_last_line = self.dimensions.width as usize;
            }

            let input_line = text.lines.last_mut().unwrap();
            input_line.spans.push(span);
        }

        text
    }

    fn wrap_lines(&self, text: Text<'a>) -> Text<'a> {
        Text {
            lines: text
                .lines
                .into_iter()
                .flat_map(|line| {
                    if line.width() > self.dimensions.width as usize {
                        self.wrap_line(line).lines
                    } else {
                        vec![line]
                    }
                })
                .collect(),
        }
    }
}
