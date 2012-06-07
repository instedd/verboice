# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

Verboice::Application.routes.draw do

  resources :contacts do
    resources :persisted_variables
    resources :recorded_audios, only: [:index, :show, :delete]
  end

  resources :channels do
    resources :queued_calls
    member do
      get :call
    end
  end

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts

  resources :projects do
    member do
      post :enqueue_call
    end
    resources :call_flows, except: [:new, :edit] do
      member do
        get :edit_workflow
        put :update_workflow
        get :play_recording
        post :save_recording
        post :import_call_flow
        get :export_call_flow
      end
    end
    resources :external_services
    resources :schedules
  end

  resources :call_logs, path: :calls do
    member do
      get :progress
      get 'results/:key', :action => :play_result, :as => 'result'
      get :download_details
    end
    collection do
      get :queued
      get :download
    end
  end

  match "api/call" => "api#call", :as => :api_call
  match "api/call/:id/state" => "api#call_state"
  match 'api/call/:id/redirect' => 'api#call_redirect'

  get "api/channels" => "api_channels#list"
  post  "api/channels" => "api_channels#create"
  delete "api/channels/:name" => "api_channels#destroy"

  get "api/schedules" => "api_schedules#index"
  get "api/schedules/:name" => "api_schedules#show"
  post "api/schedules" => "api_schedules#create"
  put "api/schedules/:name" => "api_schedules#update"
  delete "api/schedules/:name" => "api_schedules#destroy"

  root :to => 'home#index'

  get 'terms_and_conditions', :to => redirect('/')
end
