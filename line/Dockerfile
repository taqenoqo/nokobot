FROM ruby
WORKDIR /opt/line
COPY Gemfile Gemfile.lock ./
RUN bundle install
COPY app.rb ./
EXPOSE 4567
ENTRYPOINT ["bundle", "exec", "ruby", "app.rb"]

