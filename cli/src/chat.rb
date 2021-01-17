require 'rest-client'
require 'readline'
require 'json'

BOT_URL = "http://#{ENV["BOT_HOST_NAME"]}:#{ENV["BOT_PORT"]}/bot"
while input = Readline.readline("> ", true)
  bot_input = input.chomp.to_json
  response = RestClient.post(BOT_URL, bot_input, content_type: :json, accept: :json)
  bot_output = JSON.parse(response.body)
  puts(bot_output)
end

